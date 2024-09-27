#' Request Building Information from Local SQLite Database
#'
#' This function retrieves detailed information about a building from a local SQLite database.
#' The request can be made using either the building's unique identifier (EGID), its address components
#' (DEINR, STRNAME, DPLZ4), or its parcel components (LPARZ, DPLZ4). The function queries the database,
#' processes the results, and fills in the building information.
#'
#' @param building A list or data frame representing the building, containing at least EGID, address components (DEINR, STRNAME, and DPLZ4), or parcel components (LPARZ, and DPLZ4).
#' @param sqlite_conn A SQLiteConnection object representing the database connection. This must be established prior to calling this function, for efficiency purpose.
#'
#' @return A modified version of the input building object, with additional information filled in from the SQLite database.
#'         If no unique building is found, or if an error occurs during the query, the function stops with an error message.
#'
#' @examples
#' \dontrun{
#' conn <- DBI::dbConnect(RSQLite::SQLite(), "path/to/database.sqlite")
#' building <- list(EGID = 123456)
#' updated_building <- request_regbl_sqlite(building, sqlite_conn = conn)
#' DBI::dbDisconnect(conn)
#' }
#'
#' @details
#' The function constructs SQL queries to fetch data from the relevant tables in the SQLite database. After retrieving
#' the data, it processes the results to populate the building object with the appropriate types for each field, based
#' on metadata provided in `.constants$buildings_df_columns`.
#'
#' The function assumes that `.constants$buildings_df_columns` is defined and contains metadata about the expected building data fields, including their types.
#' 
#' To download the SQLite database file, please visit https://www.housing-stat.ch/fr/madd/public.html, or run the `download_regbl_db()` function.
request_regbl_data <- function(building, sqlite_conn = NULL) {
  # Connect to the SQLite database
  if (is.null(sqlite_conn)) {
    stop("Database connection not provided.")
  }

  if (is.na(building$EGID)) {
    stop("EGID not provided. Please search for the building ID using the egid_search() function.")
  }

  # Query building data using EGID
  building_query <- paste("SELECT * FROM building WHERE EGID =", building$EGID)

  building_data <- RSQLite::dbGetQuery(sqlite_conn, building_query)

  if (nrow(building_data) == 0) {
    stop("No building found with the provided EGID.")
  } else if (nrow(building_data) > 1) {
    stop("Multiple buildings found with the provided EGID.")
  }

  # Populate the building object with data
  for (col in colnames(building_data)) {
    if (col %in% names(building) && is.na(building[[col]])) {
      col_type <- .constants$buildings_df_columns[[col]]$type
      value <- building_data[[col]]

      # Convert the value based on the expected type
      if (value == "") {
        building[[col]] <- NA
      } else if (col_type == "integer") {
        building[[col]] <- as.integer(value)
      } else if (col_type == "character") {
        building[[col]] <- as.character(value)
      } else if (col_type == "double") {
        building[[col]] <- as.numeric(value)
      } else {
        building[[col]] <- value # Default case if type is not specified
      }
    }
  }

  return(building)
}
