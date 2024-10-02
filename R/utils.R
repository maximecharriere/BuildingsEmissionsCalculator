# Author: Maxime Charriere, contact@maximecharriere.ch
# Date: 02.2024
# This script contain code usefull for the package, with general purpose.

library(magrittr)

#' Euclidean Distance
#'
#' This function calculates the Euclidean distance between two points in a 2D plane.
#'
#' @param x1 The x-coordinate of the first point.
#' @param y1 The y-coordinate of the first point.
#' @param x2 The x-coordinate of the second point.
#' @param y2 The y-coordinate of the second point.
#'
#' @return The Euclidean distance between the two points.
euclidean_distance <- function(x1, y1, x2, y2) {
  sqrt((x2 - x1)^2 + (y2 - y1)^2)
}

#' Find the Closest Meteorological Station
#'
#' This function calculates the Euclidean distance between a given building
#' and a list of meteorological stations, returning the code of the closest station.
#'
#' @param climate A list of meteorological stations, where each station is an object
#'   with properties `$easting`, `$northing`, and `$code`. Easting and northing
#'   should be numeric values representing the LV95 coordinate of the station.
#' @param building An object representing the building, with properties `$GKODE`
#'   for easting and `$GKODN` for northing. Both should be numeric values representing
#'   the LV95 coordinate of the building.
#'
#' @return A character string representing the `$code` of the closest meteorological
#'   station to the given building.
find_closest_station <- function(climate, building) {
  min_distance <- Inf
  closest_station_code <- NA

  for (i in seq_along(climate)) {
    station <- climate[[i]]
    distance <- euclidean_distance(building$GKODE, building$GKODN, station$easting, station$northing)
    if (distance < min_distance) {
      min_distance <- distance
      closest_station_code <- station$code
    }
  }
  return(closest_station_code)
}

#' Standardize the Structure of a Building Data Frame
#'
#' This function ensures that a given building data frame contains a specific set of columns.
#' It checks the data frame against a provided list of column names and adds any that are missing,
#' initializing them with NA values. This standardization is essential for consistent data handling
#' and analysis, particularly when integrating multiple building data sets with potentially varying structures.
#'
#' @param buildings_df A data frame containing building information that needs to be standardized.
#' @param buildings_df_columns_names A character vector specifying the desired set of column names
#' that should be present in the buildings data frame.
#'
#' @return The input data frame augmented with any missing columns from the specified set,
#' with NA values in these new columns.
#'
#' @examples
#' buildings_df <- data.frame(EGID = c(12345, 67890), STRNAME = c("Main St", "Second St"))
#' desired_columns <- c("EGID", "STRNAME", "DEINR", "DPLZ4")
#' standardized_df <- standardize_buildings_df(buildings_df, desired_columns)
#' # standardized_df will now include DEINR and DPLZ4 columns with NA values.
#'

convert_columns_types <- function(buildings_df) {
  columns_info <- .constants$buildings_df_columns
  na_counts_before <- colSums(is.na(buildings_df))
  # Loop through the columns of buildings_df
  for (col in names(buildings_df)) {
    if (col %in% names(columns_info)) {
      # Get the type for the current column from columns_info
      col_type <- columns_info[[col]]$type

      # Perform the conversion based on the type
      buildings_df[[col]] <- switch(col_type,
        "character" = as.character(buildings_df[[col]]),
        "integer" = as.integer(buildings_df[[col]]),
        "double" = as.numeric(buildings_df[[col]]),
        "date" = as.Date(buildings_df[[col]], origin = "1970-01-01"),
        "logical" = as.logical(buildings_df[[col]]),
        stop(paste0("The type of the column '",col,"' (",col_type, ") is unknown."))
      )
    } else {
      # Print a warning if the column is not defined in columns_info
      warning(paste0("Unknown column: '", col, "' - If necessary, please add it to `constants.json`"))
    }
  }
  na_counts_dif <- colSums(is.na(buildings_df)) - na_counts_before
  if(length(na_counts_dif[na_counts_dif != 0])){
    print("NA values added! During the columns type convertion, some NA values have been added. Here are the number of added NA for each columns:")
    print(na_counts_dif[na_counts_dif>0])
  }
  return(buildings_df)
}


add_missing_columns <- function(buildings_df) {
  columns_info <- .constants$buildings_df_columns

  # Identify which of the desired columns are missing from the dataframe
  missing_columns <- setdiff(names(columns_info), names(buildings_df))

  # For each missing column, add it to the dataframe with NA values
  for (col_name in missing_columns) {
    buildings_df[[col_name]] <- NA
    warning(paste0("Column '", col_name, "' was missing and has been added with NA values."))
  }

  return(buildings_df)
}

get_column_types <- function() {
  # Extract the column types and the corresponding numeric values
  #browser()
  columns_info <- .constants$buildings_df_columns
  type_mapping <- .constants$openxlsx2_type_mapping

  # Create a named numeric vector to store the types
  types <- sapply(columns_info, function(col_info) {
    # Map the type string to its corresponding numeric value using type_mapping
    type_mapping[[col_info$type]]
  })

  return(types)
}

# Append a new error message to the existing error comments, if any
append_error_message <- function(current_errors, new_error) {
  if (is.na(current_errors)) {
    return(new_error)
  } else {
    return(paste(current_errors, new_error, sep = " | "))
  }
}

# Sanitize a string to remove disallowed characters for a filename
sanitize_filename <- function(input_string) {
  # Define a pattern of disallowed characters
  # This pattern covers common disallowed characters in Windows and Unix/Linux
  disallowed_chars_pattern <- "[\\\\/:*?\"<>|]"

  # Replace disallowed characters with an underscore
  sanitized_string <- gsub(disallowed_chars_pattern, "_", input_string)

  return(sanitized_string)
}


