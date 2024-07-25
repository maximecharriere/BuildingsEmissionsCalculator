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
egid_search <- function(building) {
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
    stop("No address found.")
  }

  # Extract weights
  weights <- sapply(results$weight, as.numeric)

  # Find the result with the highest weight
  max_weight <- max(weights)
  top_results <- results[weights == max_weight, ]

  # Check if there are multiple results with the same highest weight
  if (nrow(top_results) > 1) {
    stop("Multiple possible address found.")
  }

  # Extract the featureId and assign the EGID part to the building object
  feature_id <- top_results$attrs$featureId[1]
  building$EGID <- as.integer(strsplit(feature_id, "_")[[1]][1])

  return(building)
}
