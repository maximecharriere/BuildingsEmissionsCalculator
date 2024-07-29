library(openxlsx2)

# Load the data
wb <- wb_load("../../data/new_template_test.xlsx")

data <- wb_to_df(wb, sheet = "byEGID", startRow = 4)

data$new_column <- as.integer(data$DPLZ4) + 1000

wb <- wb_add_worksheet(wb, sheet = "byEGID", data = data, startRow = 4, colNames = TRUE)

wb_save(wb, "test.xlsx")
