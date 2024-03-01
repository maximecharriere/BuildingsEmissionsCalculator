# Load the file
INTERACTIVE <- FALSE
if (INTERACTIVE) {
  file_in <- file.choose()
} else {
  file_in <- "data/data_test.xlsx"
}
buildings_df <- readxl::read_excel(file_in, n_max = 10)

# Call the FinancedEmissionsCalculator
buildings_df <- fill_buildings_df(buildings_df)

# Save the data to a file
if (INTERACTIVE) {
  file_out <- file.choose()
} else {
  file_out <- sub("\\.xlsx$", "_filled.xlsx", file_in)
}
writexl::write_xlsx(buildings_df, file_out)
