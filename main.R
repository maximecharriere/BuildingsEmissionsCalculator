library(readxl)
library(writexl)
library(FinancedEmissionsCalculator)

# Load the file
if (.constants$interactive) {
  file <- file.choose()
} else {
  file <- "data/data_IAZI.xlsx"
}

# Load the data from the file in a data frame
buildings_df <- read_excel(file, n_max = 100)

buildings_df <- fill_dataframe(buildings_df)

# Save the data to a file
if (.constants$interactive) {
  file <- file.choose()
} else {
  file <- "output/data_IAZI_filled_100.xlsx"
}

write_xlsx(buildings_df, file)
