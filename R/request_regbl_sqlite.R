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
#' The function first determines the type of query based on available information in the `building` object:
#' - If EGID is available, it retrieves information based on the building's EGID.
#' - If EGID is not available, but address components are, it retrieves information based on the building's address.
#' - If neither EGID nor address components are available, but parcel components are, it retrieves information based on the building's parcel.
#'
#' The function constructs SQL queries to fetch data from the relevant tables in the SQLite database. After retrieving
#' the data, it processes the results to populate the building object with the appropriate types for each field, based
#' on metadata provided in `.constants$buildings_df_columns`.
#'
#' The function assumes that `.constants$buildings_df_columns` is defined and contains metadata about the expected building data fields, including their types.
#' 
#' To download the SQLite database file, please visit https://www.housing-stat.ch/fr/madd/public.html
request_regbl_sqlite <- function(building, sqlite_conn = NULL) {
  # Connect to the SQLite database
  if (is.null(sqlite_conn)) {
    stop("Database connection not provided.")
  }

  # Step 1: Determine EGID if not provided
  if (is.na(building$EGID)) {
    if (!is.na(building$DEINR) && !is.na(building$STRNAME) && !is.na(building$DPLZ4)) {
      # Search by address
      query <- paste(
        "SELECT EGID FROM entrance WHERE DPLZ4 =", shQuote(building$DPLZ4),
        "AND STRNAME =", shQuote(building$STRNAME),
        "AND DEINR =", shQuote(building$DEINR)
      )
      result <- RSQLite::dbGetQuery(sqlite_conn, query)
    } else if (!is.na(building$LPARZ) && !is.na(building$DPLZ4)) {
      # Search by parcel
      query_building <- paste("SELECT EGID FROM building WHERE LPARZ =", shQuote(building$LPARZ))
      result_building <- RSQLite::dbGetQuery(sqlite_conn, query_building)

      if (nrow(result_building) == 0) {
        stop("No building found with the provided LPARZ.")
      }

      egids <- paste(result_building$EGID, collapse = ", ")
      query_entrance <- paste(
        "SELECT EGID FROM entrance WHERE EGID IN (", egids, ")",
        "AND DPLZ4 =", shQuote(building$DPLZ4)
      )
      result <- RSQLite::dbGetQuery(sqlite_conn, query_entrance)
    } else {
      stop("Building data is incomplete. Need EGID, or DEINR/STRNAME/DPLZ4, or LPARZ to find the building in the database.")
    }

    if (nrow(result) == 0) {
      stop("No building found with the provided information.")
    } else if (nrow(result) > 1) {
      stop("Multiple buildings found; please refine your search criteria.")
    }

    building$EGID <- result$EGID[1]
  }

  # Step 2: Query data from both tables using EGID
  building_query <- paste("SELECT * FROM building WHERE EGID =", building$EGID)

  building_data <- RSQLite::dbGetQuery(sqlite_conn, building_query)

  if (nrow(building_data) == 0) {
    stop("No building found with the provided EGID.")
  } else if (nrow(building_data) > 1) {
    stop("Multiple buildings found with the provided EGID.")
  }

  # Step 3: Populate the building object with data
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
