# Load test data
library(openxlsx2)
file_in <- system.file("extdata", "test_data.xlsx", package = "BuildingsEmissionsCalculator")
regbl_db_path <- "../../data/regbl_db.sqlite"

wb <- wb_load(file_in)
data_tables_info <- wb_get_tables(wb, sheet = "byBuilding")
data_table_pos <- data_tables_info$tab_ref[data_tables_info$tab_name == "data_table"]
buildings_df <- wb_to_df(wb, sheet = "byBuilding", dims = data_table_pos)

# run test
buildings_df <- BuildingsEmissionsCalculator::fill_buildings_df(buildings_df, data_source='sqlite', regbl_db_path = regbl_db_path)
