# File to convert WGS84 coordinates to LV95 of the climate stations
load("data/climate.rda")

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
save(climate, file = "data/climate.rda")


