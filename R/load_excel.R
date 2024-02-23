# TODO 2nd heating and 2nd hot water system not taken into account

# Load packages
# library(readxl)
# library(co2calculatorPACTA2022)
library(jsonlite)
# library(FinancedEmissionsCalculator)

 
# # Load the file
# if (INTERACTIVE) {
#   file <- file.choose()
# } else {
#   file <- "path/to/file.xlsx"
# }
fill_building_data <- function() {
  # Load constant from json file
  constants <- fromJSON("data/constants.json")

  # print(file)

  # Test address
  building <- list()
  building$DEINR <- 8
  building$STRNAME <- "Rue des Diamants"
  building$DPLZ4 <- 2503

  # Call the regbl API
  building <- request_regbl(building)

  # Prepare the building data for the co2calculator
  # If the energy reference surface is not available, calculate it from the gross area times the number of floors.
  if (is.na(building$GEBF)) {
    building$GEBF <- building$GAREA * building$GASTW
  }
  # Convert the building class from RegBl to the SIA 380/1 standard
  utilisation_key <- constants$buildingClassRegbl2Sia[as.character(building$GKLAS)]
  climate_code <- find_closest_station(climate, building)
  print(climate_code)


  # calculate_emissions(
  #     area <- building$GAREA,
  # floors <- building$GASTW,
  # year <- building$GBAUJ,
  # utilisation_key <- utilisation_key,
  # climate_code <- climate_code,
  # energy_carrier <-)
  # print(building)
}

