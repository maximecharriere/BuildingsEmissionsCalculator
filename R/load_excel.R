# TODO 2nd heating and 2nd hot water system not taken into account

# Load packages
library(readxl)

# Constant
INTERACTIVE <- TRUE

# Load the file
if (INTERACTIVE) {
  file <- file.choose()
} else {
  file <- "path/to/file.xlsx"
}

print(file)

# Test address
building <- list()
building$DEINR <- 8
building$STRNAME <- "Rue des Diamants"
building$DPLZ4 <- 2503

# Call the regbl API
building <- request_regbl(building)

print(building)