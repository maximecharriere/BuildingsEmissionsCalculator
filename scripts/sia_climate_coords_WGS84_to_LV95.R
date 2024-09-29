# Author: Maxime Charriere, contact@maximecharriere.ch
# Date: 02.2024
# This script is used to transform the WGS84 coordinates of the climate stations in
# the co2calculatorPACTA2022::climate dataset to LV95 coordinates.
# Used during the dev phase.

library(magrittr)

# --------------------- #
# ----- FUNCTIONS ----- #
# --------------------- #

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

# --------------------- #
# ------- MAIN -------- #
# --------------------- #

# Loop through each station in the climate list
for (i in seq_along(climate)) {
  # Extract WGS84 coordinates for the current station
  easting <- climate[[i]]$longitude
  northing <- climate[[i]]$latitude
  altitude <- climate[[i]]$altitude

  # Call the API to convert coordinates
  converted_coords <- wgs84_to_lv95(easting, northing, altitude)

  # Add the converted coordinates to the station's properties
  climate[[i]]$easting <- converted_coords$easting
  climate[[i]]$northing <- converted_coords$northing
  climate[[i]]$altitudeBessel <- converted_coords$altitude
}

# Save the updated climate list to a file
save(climate, file = "data/climate_LV95.rda")

# This file is then moved to the "data" folder of the co2calculatorPACTA2022 package.
