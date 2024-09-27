egid_search <- function(building, sqlite_conn = NULL) {
  if (is.null(sqlite_conn)) {
    stop("Database connection not provided.")
  }

  if (!is.na(building$EGID)) {
    return(building)
  }

  # Split the address into components if the address is not already split
  if (is.na(building$STRNAME) && is.na(building$DEINR) && !is.na(building$ADDRESS)) {
    building <- split_address(building)
  }
  
  # Search for the EGID in the SQLite database by street name, number, and postal code
  search1_egids <- splitted_address_search(building, sqlite_conn)
  if (length(search1_egids) == 1) {
    building$EGID <- search1_egids[1]
    return(building)
  }

  # Search for the EGID in the SQLite database by parcel number and postal code
  search2_egids <- parcel_search(building, sqlite_conn)
  if (length(search2_egids) == 1) {
    building$EGID <- search2_egids[1]
    return(building)
  }

  # If the EGID is not found, search using the Swiss GeoAdmin API
  search3_egids <- oneline_address_search(building)
  if (length(search3_egids) == 1) {
    building$EGID <- search3_egids[1]
    return(building)
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

splitted_address_search <- function(building, sqlite_conn) {
  if (!is.na(building$DEINR) && !is.na(building$STRNAME) && !is.na(building$DPLZ4)) {
    # Search by address
    query <- paste(
      "SELECT EGID FROM entrance WHERE DPLZ4 =", shQuote(building$DPLZ4),
      "AND STRNAME =", shQuote(building$STRNAME),
      "AND DEINR =", shQuote(building$DEINR)
    )
    result <- RSQLite::dbGetQuery(sqlite_conn, query)

    return(unique(result$EGID)) # return EGID without duplicates
  }

  return(character()) # return empty character vector
}

parcel_search <- function(building, sqlite_conn) {
  if (!is.na(building$LPARZ) && !is.na(building$DPLZ4)) {
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

  return(character()) # return empty character vector
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
  response <- httr::GET(url, query = params)

  # Check if the request was successful
  if (httr::status_code(response) != 200) {
    stop("Failed to retrieve data from the API.")
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
