library(magrittr)

#' pkg_install
#'
#' Install one or more package from a given CRAN repository,
#' is they're not already installed.
#'
#' @param packages Vector of packages to install.
#' @param repos Link to the CRAN repo to get the packages.
#'
#' @examples
#' pkg_install(packages = c("knitr", "readxl"), repos = "https://stat.ethz.ch/CRAN")
pkg_install <- function(packages, repos = "https://stat.ethz.ch/CRAN") {
  packagecheck <- match(packages, utils::installed.packages()[, 1])
  packagestoinstall <- packages[is.na(packagecheck)]
  if (length(packagestoinstall) > 0L) {
    utils::install.packages(packagestoinstall,
      repos = repos
    )
  } else {
    print("All requested packages already installed")
  }
}

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
#'
#' @export
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
#' @param building An object representing the building, with properties `$DKODE`
#'   for easting and `$DKODN` for northing. Both should be numeric values representing
#'   the LV95 coordinate of the building.
#'
#' @return A character string representing the `$code` of the closest meteorological
#'   station to the given building.
#'
#' @export
find_closest_station <- function(climate, building) {
  min_distance <- Inf
  closest_station_code <- NA

  for (i in seq_along(climate)) {
    station <- climate[[i]]
    distance <- euclidean_distance(building$DKODE, building$DKODN, station$easting, station$northing)
    if (distance < min_distance) {
      min_distance <- distance
      closest_station_code <- station$code
    }
  }
  return(closest_station_code)
}

#' Call Geodesy API to Convert WGS84 to LV95 Coordinates
#'
#' This function sends a request to the Geodesy API to convert coordinates from WGS84 to LV95.
#'
#' @param easting Ellipsoidal longitude in decimal degrees [°] on WGS84.
#' @param northing Ellipsoidal latitude in decimal degrees [°] on WGS84.
#' @param altitude Ellipsoidal height in meters [m] on WGS84. If not required, set to `NULL`.
#' @return A list containing the easting, northing, and altitude in LV95 coordinate system.
#'         If the altitude is specified, it returns the ellipsoidal height on Bessel.
#' @references Geodesy API documentation: https://www.swisstopo.admin.ch/en/rest-api-geoservices-reframe-web
#'
#' @export
wgs84_to_lv95 <- function(easting, northing, altitude) {
  # Base URL of the API
  base_url <- "https://geodesy.geo.admin.ch/reframe/wgs84tolv95"

  # Make the GET request using httr2
  response <- httr2::request(base_url) %>%
    httr2::req_url_query(easting = easting, northing = northing, altitude = altitude, format = "json") %>%
    httr2::req_perform()

  # Check the status code of the response
  if (httr2::resp_status(response) == 200) {
    # Parse the JSON response
    parsed_response <- response %>%
      httr2::resp_body_json(simplifyVector = TRUE) %>%
      lapply(function(x) as.numeric(x))
    return(parsed_response)
  } else {
    # Handle error based on status code
    stop("Error in API call. HTTP status code: ", httr2::resp_status(response))
  }
}

#' Split Address into Street Name and House Number
#'
#' This function takes a single string address and splits it into two components: the street name and the house number. The street is everything before the last sequence of digits which may include letters (e.g., "54A" or "10A").
#'
#' @param address A single string containing a street name followed by a house number.
#' @return A list with two elements: $street containing the street name, and $number containing the house number.
#' @export
split_address <- function(address) {
  # This pattern looks for any sequence of digits (\d+) possibly followed by non-digits (\D*)
  # at the end of the string ($)
  pattern <- "^(.*?)(\\d+\\w*)?$$"

  # Use strcapture to extract the parts
  parts <- utils::strcapture(pattern, address, proto = list(STRNAME = character(), DEINR = character()))

  # Trim leading and trailing spaces from the street name
  parts$STRNAME <- trimws(parts$STRNAME)

  return(parts)
}

#' Split Addresses into Street Name and House Number in a Data Frame
#'
#' This function takes a data frame with a column containing addresses and splits each address into two components: the street name and the house number. The street is everything before the last sequence of digits which may include letters (e.g., "54A" or "10A").
#'
#' @param data A data frame containing a column with addresses.
#' @param col_name The name of the column containing the addresses.
#' @return A data frame with the original addresses and two new columns: $street containing the street name, and $number containing the house number.
#' @export
split_addresses <- function(data, col_name = "Addresses") {
  data <- cbind(data, do.call(rbind, lapply(data[[col_name]], split_address)))
  return(data)
}


add_missing_columns <- function(df) {
  # Identify which of the desired columns are missing from the dataframe
  desired_columns <- c(
    # Required
    "DEINR", # numeric The building entrance number.
    "STRNAME", # character The street name.
    "DPLZ4", # numeric The postal code.
    "EGID", # numeric The unique building identifier.
    "GKLAS", # numeric The building class.
    "GBAUJ", # numeric The year of construction.
    "GBAUP", # numeric The period of construction.
    "GABBJ", # numeric The year of demolition.
    "GAREA", # numeric The surface area of the building.
    "GASTW", # numeric The number of floors.
    "GEBF", # numeric The energy relevant surface of the building.
    "DKODE", # numeric The building easting coordinate in the LV95 coordinate system.
    "DKODN", # numeric The building northing coordinate in the LV95 coordinate system.
    "GENH1", # character The energy source for heating 1.
    "GWAERDATH1", # Date The revision date for heating 1.
    "energy_relevant_area", # numeric The energy relevant surface of the building.
    "floors", # numeric The number of floors.
    "year", # numeric The year of construction.
    "utilisation_key", # numeric The utilisation key according to SIA 380/1.
    "climate_code", # numeric The climate code of the closest climate station.
    "energy_carrier", # character The energy carrier according to SIA 380/1.
    "walls_refurb_year", # numeric The year of the last refurbishment of the walls.
    "roof_refurb_year", # numeric The year of the last refurbishment of the roof.
    "windows_refurb_year", # numeric The year of the last refurbishment of the windows.
    "floor_refurb_year", # numeric The year of the last refurbishment of the floor.
    "heating_install_year", # numeric The year of the installation of the heating system.
    "heatEnergy", # numeric Annual heat energy required per area, in MJ/m2 per year.
    "emissionCoefficient", # numeric Applied CO2 emission coefficient according to the given energy carrier, in kg/MJ.
    "emissionsPerArea", # numeric Annual CO2 emissions per area, in kg/m2 per year.
    "emissionsTotal", # numeric Total annual CO2 emissions, in kg per year.
    "error_comments", # character A string containing error messages.
    # Others
    "GKAT", # numeric The building category.
    "GWAERZH1", # character The heat generator for heating 1.
    "GWAERZH2", # character The heat generator for heating 2.
    "GENH2", # character The energy source for heating 2.
    "GWAERSCEH1", # character The information source for heating 1.
    "GWAERSCEH2", # character The information source for heating 2.
    "GWAERDATH2", # Date The revision date for heating 2.
    "GWAERZW1", # character The heat generator for warm water 1.
    "GWAERZW2", # character The heat generator for warm water 2.
    "GENW1", # character The energy source for warm water 1.
    "GENW2", # character The energy source for warm water 2.
    "GWAERSCEW1", # character The information source for warm water 1.
    "GWAERSCEW2", # character The information source for warm water 2.
    "GWAERDATW1", # Date The revision date for warm water 1.
    "GWAERDATW2" # Date The revision date for warm water 2.
  )
  missing_columns <- setdiff(desired_columns, names(df))

  # For each missing column, add it to the dataframe with NA values
  for (col in missing_columns) {
    df[[col]] <- NA
  }

  return(df)
}

append_error_message <- function(current_errors, new_error) {
  if (is.na(current_errors)) {
    return(new_error)
  } else {
    return(paste(current_errors, new_error, sep = " | "))
  }
}

sanitize_filename <- function(input_string) {
  # Define a pattern of disallowed characters
  # This pattern covers common disallowed characters in Windows and Unix/Linux
  disallowed_chars_pattern <- "[\\\\/:*?\"<>|]"

  # Replace disallowed characters with an underscore
  sanitized_string <- gsub(disallowed_chars_pattern, "_", input_string)

  return(sanitized_string)
}
