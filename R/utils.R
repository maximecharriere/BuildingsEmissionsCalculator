library(httr2)

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

# Function to calculate Euclidean distance between two points
euclidean_distance <- function(x1, y1, x2, y2) {
  sqrt((x2 - x1)^2 + (y2 - y1)^2)
}

#' Find the Closest Meteorological Station
#'
#' This function calculates the Euclidean distance between a given building
#' and a list of meteorological stations, returning the code of the closest station.
#'
#' @param climate A list of meteorological stations, where each station is an object
#'   with properties `$latitude`, `$longitude`, and `$code`. Latitude and longitude
#'   should be numeric values representing the station's location.
#' @param building An object representing the building, with properties `$DKODE`
#'   for latitude and `$DKODN` for longitude. Both should be numeric values.
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
  response <- request(base_url) %>%
    req_url_query(easting = easting, northing = northing, altitude = altitude, format = "json") %>%
    req_perform()
  
  # Check the status code of the response
  if (resp_status(response) == 200) {
    # Parse the JSON response
    parsed_response <- response %>% resp_body_json(simplifyVector = TRUE) %>% lapply(function(x) as.numeric(x))
    return(parsed_response)
  } else {
    # Handle error based on status code
    stop("Error in API call. HTTP status code: ", resp_status(response))
  }
}
