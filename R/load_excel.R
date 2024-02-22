# TODO 2nd heating and 2nd hot water system not taken into account

# Load packages
library(readxl)
library(co2calculatorPACTA2022)

# # Constant
# INTERACTIVE <- TRUE

# # Load the file
# if (INTERACTIVE) {
#   file <- file.choose()
# } else {
#   file <- "path/to/file.xlsx"
# }

# print(file)

# Test address
building <- list()
building$DEINR <- 8
building$STRNAME <- "Rue des Diamants"
building$DPLZ4 <- 2503

# Call the regbl API
building <- request_regbl(building)

# If the energy reference surface is not available, calculate it from the gross area times the number of floors.
if (is.na(building$GEBF)) {
  building$GEBF <- building$GAREA * building$GASTW
}

calculate_emissions(
    area <- building$GAREA, 
floors <- building$GASTW,
year <- building$GBAUJ,
utilisation_key <- )
# print(building)
