devtools::load_all(".")


file_in <- "C:/Users/Maxime/Swiss Climate AG/Projet Hypothèques - General/10_Software/data/address_test.xlsx"

buildings_df <- readxl::read_excel(file_in)
buildings_df <- standardize_buildings_df(buildings_df, names(.constants$buildings_df_columns))

# Call the BuildingsEmissionsCalculator on row where emissionsTotal is NA
fill_building_data(buildings_df[1, ])

# file_in <- "C:/Users/Maxime/Swiss Climate AG/Projet Hypothèques - General/22_RegBl/databse/ch-202406201455/gebaeude_batiment_edificio.csv"
# # buildings_df <- read.csv(file_in, sep = "\t")

# print(buildings_df[buildings_df$LPARZ == 2207 & buildings_df$GGDENAME == "Biel/Bienne", ])

# print(colnames(buildings_df))