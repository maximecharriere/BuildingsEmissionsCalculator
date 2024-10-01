# Author: Maxime Charriere, contact@maximecharriere.ch
# Date: 02.2024


################
# Load BuildingsEmissionsCalculator package #
################

devtools::load_all(".")
packageDescription('BuildingsEmissionsCalculator', fields=c("Title", "Version"))

##############
# Parameters #
##############

# The Excel file must follow the "05_Templates/template_<version>.xlsx" template
INTERACTIVE <- FALSE # FALSE / TRUE # Choose if the File Explorer is used to search Excel file, or if the file path is directly written in the R script.
excel_filepath <- "tests/testdata/tests_0.4.1.xlsx" #R ne comprend pas les backslash --> remplacer le filepath par double backslash ou slash normal
regbl_database_path <- "data/regbl_db.sqlite"
max_rows = -1 # The number of row to fill. Put -1 to fill the entire table.

#######################
# Load the Excel data #
#######################

# Select the input file path
if (INTERACTIVE) {
  file_in <- file.choose() # Use File Explorer
} else {
  file_in <- excel_filepath
}

# Check that the file exist
if (!file.exists(file_in)){
  stop(paste0("No file \"", file_in, "\" found."))
}

# Create a backup of the file
file_save <- sub("\\.xlsx$", "_backup.xlsx", file_in)
file.copy(file_in, file_save)

# Load the workbook
wb <- openxlsx2::wb_load(file_in)

# Get the list of data tables
tables_list <- openxlsx2::wb_get_tables(wb, sheet = "byBuilding")

# Get the buildings_table position
buildings_table_pos <- tables_list$tab_ref[tables_list$tab_name == "buildings_table"]

# Read the buildings_table
buildings_df <- openxlsx2::wb_to_df(wb, sheet = "byBuilding", dims = buildings_table_pos)


#####################################
# Call BuildingsEmissionsCalculator #
#####################################

# Call the BuildingsEmissionsCalculator on row where emissionsTotal is NA
execution_time <- system.time({
  if (max_rows>0){
    buildings_df[1:max_rows,] <- BuildingsEmissionsCalculator::fill_buildings_df(buildings_df[1:max_rows,], regbl_db_path = regbl_database_path)
  }
  else {
    buildings_df <- BuildingsEmissionsCalculator::fill_buildings_df(buildings_df, regbl_db_path = regbl_database_path)
  }
})
print(execution_time)

#######################
# Save the Excel data #
#######################

# Select the output file path
file_out <- sub("\\.xlsx$", paste0("_filled_", nrow(buildings_df), ".xlsx"), file_in)

# remove the previous data table from the worksheet
wb <- openxlsx2::wb_remove_tables(wb, sheet = "byBuilding", table = "buildings_table")

# write the data back to the data table
wb <- openxlsx2::wb_add_data_table(wb, x = buildings_df, dims = buildings_table_pos, sheet = "byBuilding", table_name = "buildings_table", na.strings = "")

# Save the workbook
openxlsx2::wb_save(wb, file_out)

# Open the Excel file
shell(file.path(getwd(), file_out), wait = FALSE)

