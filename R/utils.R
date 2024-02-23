# Function to calculate Euclidean distance between two points
euclidean_distance <- function(lat1, lon1, lat2, lon2) {
  sqrt((lat2 - lat1)^2 + (lon2 - lon1)^2)
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
  
  for (i in 1:length(climate)) {
    station <- climate[[i]]
    distance <- euclidean_distance(building$DKODE, building$DKODN, station$latitude, station$longitude)
    
    if (distance < min_distance) {
      min_distance <- distance
      closest_station_code <- station$code
    }
  }
  
  return(closest_station_code)
}