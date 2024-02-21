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
