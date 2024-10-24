egid_search <- function(building, sqlite_conn = NULL, sqlite_address_search = TRUE, sqlite_parcel_search = TRUE, geoadmin_api_search = TRUE) {
  if (is.null(sqlite_conn)) {
    stop("RegBl SQLite Database connection not provided.")
  }

  if (!is.na(building$EGID)) {
    building$log_comments <- append_log(building$log_comments, "EGID already provided.")
    return(building)
  }

  # 0: Split the address into components if the address is not already split
  if (is.na(building$STRNAME) && is.na(building$DEINR) && !is.na(building$ADDRESS)) {
    building$log_comments <- append_log(building$log_comments, "Step 0: Splitting ADDRESS into STRNAME and DEINR.")

    building <- split_address(building)
    building$log_comments <- append_log(building$log_comments, paste0("Splitting complete. ADDRESS: '", building$ADDRESS, "' splitted in STRNAME: '", building$STRNAME, "' and DEINR: '", building$DEINR, "'."))
  }

  # 1: Search for the EGID in the SQLite database by street name, number, and postal code
  search1_egids <- list()
  if (sqlite_address_search){
    building$log_comments <- append_log(building$log_comments, "Step 1: Searching for EGID by street name, number, and postal code in the SQLite RegBl Database.")
    search1_egids <- address_search(building, sqlite_conn)
    building$log_comments <- append_log(building$log_comments, paste0("Found ", length(search1_egids), " EGIDs."))

    if (length(search1_egids) == 1) {
      building$EGID <- search1_egids[1]
      return(building)
    }
  }

  # 2: Search for the EGID in the SQLite database by parcel number and postal code
  search2_egids <- list()
  if (sqlite_parcel_search){
    building$log_comments <- append_log(building$log_comments, "Step 2: Searching for EGID by parcel number and postal code in the SQLite RegBl Database.")
    search2_egids <- parcel_search(building, sqlite_conn)
    building$log_comments <- append_log(building$log_comments, paste0("Found ", length(search2_egids), " EGIDs."))

    if (length(search2_egids) == 1) {
      building$EGID <- search2_egids[1]
      return(building)
    }
  }

  # 3: If the EGID is not found, search using the Swiss GeoAdmin API
  search3_egids <- list()
  if (geoadmin_api_search){
    building$log_comments <- append_log(building$log_comments, "Step 3: Searching for EGID using the Swiss GeoAdmin API.")
    search3_egids <- oneline_address_search(building)
    building$log_comments <- append_log(building$log_comments, paste0("Found ", length(search3_egids), " EGIDs."))

    if (length(search3_egids) == 1) {
      building$EGID <- search3_egids[1]
      return(building)
    }
  }

  # If no unique EGID is found with one of the methods, we count the occurrences of each unique EGID. The one with the highest count is selected.
  combined_egids <- c(search1_egids, search2_egids, search3_egids)

  if (length(combined_egids) == 0) {
    stop("No EGID found.")
  }

  egids_counts <- table(combined_egids)
  max_count <- max(egids_counts)
  most_common_egids <- names(egids_counts[egids_counts == max_count])
  if (length(most_common_egids) == 1) {
    building$EGID <- most_common_egids[1]
    return(building)
  } else if (length(most_common_egids) > 1) {
    stop("Multiple EGIDs found.")
  } else {
    stop("Unreachable exception.")
  }
}


split_address <- function(building) {
  # trim whitespace
  building$ADDRESS <- stringr::str_trim(building$ADDRESS)
  # Extract numbers at the end of the address, which are likely to be address numbers
  numbers <- stringr::str_extract_all(building$ADDRESS, "\\d+[A-Za-z]?(?=(\\s*,\\s*\\d+[A-Za-z]?)*$)")
  # Join the numbers into a single string
  strnum <- paste(unlist(numbers), collapse = ", ")
  if (is.na(strnum) || strnum == "") strnum <- NA # Return NA if no street number is found
  building$DEINR <- strnum

  # Remove the address numbers from the original address to get the street name
  strname <- stringr::str_extract(building$ADDRESS, ".+?(?=(\\s+\\d+[A-Za-z]?)?(\\s*,\\s*\\d+[A-Za-z]?)*\\s*$)")
  # Trim whitespace
  strname <- stringr::str_trim(strname)
  if (is.na(strname) || strname == "") strname <- NA # Return NA if no street name is found
  building$STRNAME <- strname

  return(building)
}

address_search <- function(building, sqlite_conn) {
  if (is.na(building$DEINR) || is.na(building$STRNAME) || is.na(building$DPLZ4)) {
    building$log_comments <- append_log(building$log_comments, "Address components missing (DEINR, STRNAME or DPLZ4). Skipping SQLite address search.")
    return(character())
  }

  # Search by address
  query <- paste(
    "SELECT EGID FROM entrance WHERE DPLZ4 =", shQuote(building$DPLZ4),
    "AND STRNAME =", shQuote(building$STRNAME),
    "AND DEINR =", shQuote(building$DEINR)
  )
  result <- RSQLite::dbGetQuery(sqlite_conn, query)

  return(unique(result$EGID)) # return EGID without duplicates
}


parcel_search <- function(building, sqlite_conn) {
  if (is.na(building$LPARZ) || is.na(building$DPLZ4)) {
    building$log_comments <- append_log(building$log_comments, "Parcel components missing (LPARZ or DPLZ4). Skipping SQLite parcel search.")
    return(character())
  }
  # Search by parcel
  query_building <- paste("SELECT EGID FROM building WHERE LPARZ =", shQuote(building$LPARZ))
  result_building <- RSQLite::dbGetQuery(sqlite_conn, query_building)

  if (nrow(result_building) == 0) {
    return(character())
  }

  egids <- paste(result_building$EGID, collapse = ", ")
  query_entrance <- paste(
    "SELECT EGID FROM entrance WHERE EGID IN (", egids, ")",
    "AND DPLZ4 =", shQuote(building$DPLZ4)
  )
  result_entrance <- RSQLite::dbGetQuery(sqlite_conn, query_entrance)
  if (nrow(result_entrance) == 0) {
    return(character())
  }

  return(unique(result_entrance$EGID)) # return EGID without duplicates
}




#' Retrieve EGID from Swiss GeoAdmin API Based on Address Information
#'
#' This function queries the Swiss GeoAdmin API to find the EGID (unique building identifier)
#' based on provided address information. It constructs a search query using the building's
#' postal code (DPLZ4), street name (STRNAME), and house number (DEINR). The function retrieves
#' the most likely matching address and extracts the EGID from the result.
#'
#' @param building A list containing optional address components: DPLZ4 (postal code), STRNAME (street name), and DEINR (house number).
#'                 These components are used to search the corresponding address.
#'
#' @return The input `building` list with an additional field `EGID`, which contains the unique identifier of the building.
#'
#' @examples
#' building <- list(STRNAME = "Taubenstrasse 32 3011 Bern")
#' building <- egid_search(building)
#'
oneline_address_search <- function(building) {
  # Concatenate the components to form the search text
  search_text <- paste(na.omit(c(building$DPLZ4, building$STRNAME, building$DEINR)), collapse = " ")

  # Construct the API URL
  url <- "https://api3.geo.admin.ch/rest/services/api/SearchServer"

  # Parameters for the API request
  params <- list(
    type = "locations",
    origins = "address",
    searchText = search_text,
    returnGeometry = "false"
  )

  # Perform the API request
  response <- tryCatch({
    httr::GET(url, query = params, httr::timeout(10))  # 10-second timeout to avoid long wait times
  }, error = function(e) {
    stop("Failed to retrieve data from the Geo Admin API due to a connection issue: ", e$message, "\nTurn off the `geoadmin_api_search` parameter to skip this step.")
  })

  # Check if the request was successful
  if (httr::status_code(response) != 200) {
    stop("Failed to retrieve data from the API. Status code: ", httr::status_code(response), "\nTurn off the `geoadmin_api_search` parameter to skip this step.")
  }

  # Parse the JSON response
  data <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))

  # Extract the results
  results <- data$results

  # Check for empty results
  if (length(results) == 0) {
    return(character())
  }

  # Extract weights
  weights <- sapply(results$weight, as.numeric)

  # Find the result with the highest weight
  max_weight <- max(weights)
  top_results <- results[weights == max_weight, ]

  # Extract the featureId and assign the EGID part to the building object
  feature_ids <- top_results$attrs$featureId

  # Extract the egid number from the feature ID. It can have the structure "536170_0" or "536170", so we split by "_" and take the first part
  EGID <- sapply(strsplit(feature_ids, "_"), `[`, 1)
  return(unique(EGID)) # return EGID without duplicates
}
