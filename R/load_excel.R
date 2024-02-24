# TODO 2nd heating and 2nd hot water system not taken into account

# Load packages
# library(readxl)
library(co2calculatorPACTA2022)
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

  # TODO Test address
  building <- list()
  building$DEINR <- 8
  building$STRNAME <- "Rue des Diamants"
  building$DPLZ4 <- 2503

  # Call the regbl API to get the building data
  building <- request_regbl(building)

  ## Prepare the building data for the co2calculator
  # If the energy reference surface is not available, calculate it from the gross area times the number of floors.
  if (is.na(building$GEBF)) {
    building$GEBF <- building$GAREA * building$GASTW
  }
  # If the year of construction is not available in RegBl,
  # use the smallest year of the period of construction given by RegBl.
  # If the period of construction is not available in RegBl, use 1900.
  if (is.na(building$GBAUJ)) {
    building$GBAUJ <- constants$constructionYear_RegblPeriod2Year[[as.character(building$GBAUP)]]
    if (is.na(building$GBAUJ)) {
      building$GBAUJ <- 1900
    }
  }
  # Find the closest climate station in the 40 listed in SIA 2028:2010 https://www.sia.ch/fileadmin/content/download/sia-norm/korrigenda_sn/2028-C1_2015_d.pdf
  building$climate_code <- find_closest_station(climate, building)
  # Convert the building class from RegBl to the SIA 380/1 standard
  building$utilisation_key <- constants$buildingClass_Regbl2Sia[[as.character(building$GKLAS)]]
  # Convert the energy carrier type from RegBl to the SIA 380/1 standard
  building$energy_carrier <- constants$energyCarrier_Regbl2Sia[[as.character(building$GENH1)]]
  if (is.na(building$energy_carrier)) {
    building$energy_carrier <- "other"
  }
  save(building, file = "data/building.RData")

  # result <- calculate_emissions(
  #   area = building$GEBF,
  #   floors = building$GASTW,
  #   year = building$GBAUJ,
  #   utilisation_key = building$utilisation_key,
  #   climate_code = building$climate_code,
  #   energy_carrier = building$energy_carrier
  # )
}
