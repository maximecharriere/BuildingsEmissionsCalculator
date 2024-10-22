# Author:   Annika Schmidt
# Content: Analysis Financed emissions of mortgages BCVs


# LOAD PACKAGES ############################################
install.packages("pacman")
install.packages("openxlsx")
install.packages("knitr")
pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes,
               ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny,
               stringr, tidyr)
library(datasets)
library(openxlsx)
library(ggplot2)
library(dplyr)
library(knitr)

# IMPORT FILE ############################################

# Excel XLSX
INTERACTIVE <- TRUE
if (INTERACTIVE) {
  file_in <- file.choose() # Use File Explorer
} else {
  file_in <- excel_filepath
}
wb <- openxlsx2::wb_load(file_in)

# Read the excel file
data <- openxlsx2::wb_to_df(wb, sheet = "byBuilding", start_row = 5, col_names = TRUE)
data_subset <- data[1:31746, ]
# Check the new dimensions
cat("Number of rows in the subset:", nrow(data_subset), "\n")

# ERROR COMMENTS ############################################
# Group by ErrorComments and count occurrences
error_summary <- data_subset %>%
  filter(!is.na(error_comments)) %>%
  group_by(error_comments) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

# View the summary
print(error_summary)

#### Check does not work
data_subset <- data %>%
  mutate(
    check_equal = ifelse(!is.na(emissions_per_area) & !is.na(asset_emissions) & !is.na(asset_energetic_area),
                         emissions_per_area == (asset_emissions / asset_energetic_area),
                         NA)
  )

summary(data$check_equal)

### Check for outliers
# Boxplots
#It displays the distribution of data based on quartiles and highlights outliers as points outside the whiskers.
ggplot(data_subset, aes(y = emissions_per_area)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Boxplot with Outliers", y = "Emmissions per area") +
  theme_minimal()

# Density plot
ggplot(data_subset, aes(x = emissions_per_area)) +
  geom_density(fill = "lightblue", alpha = 0.5) +
  geom_rug(sides = "b", color = "red") +
  labs(title = "Density Plot with Outliers", x = "Emissions per area") +
  theme_minimal()

# Identify outliers
#lower_bound <- 0
#upper_bound <- 350
#outliers <- data_subset$emissions_per_area[data_subset$emissions_per_area < lower_bound | data_subset$emissions_per_area > upper_bound]

# Print outliers
#print(outliers)

# Filter out outliers
#data_subset <- data_subset %>%
# filter(emissions_per_area >= lower_bound & emissions_per_area <= upper_bound)

### OR

# Mutate observations where 'emissions_per_area' is greater 150
data_subset_reduced <- data %>%
  mutate(emissions_per_area = ifelse(emissions_per_area > 150, NA, emissions_per_area))

# Calculate emissions ############################################
#data_subset$financed_emissions <- data_subset$asset_emissions*data_subset$bank_share
#data_subset$financed_area <- data_subset$asset_emissions * data_subset$bank_share*data_subset$energy_relevant_area

# Create values ############################################

# Total surface of the assets (m2)
total_asset_energetic_area <- sum(data_subset$asset_energetic_area, na.rm = TRUE)

# total asset value (CHF)
total_asset_value <- sum(data_subset$asset_value, na.rm = TRUE)
# total mortgage value (CHF)
total_mortgage_value <- sum(data_subset$mortgage_value, na.rm = TRUE)

# total asset emissions (absolute, in kg CO2)
total_asset_emissions <- sum(data_subset$asset_emissions, na.rm = TRUE)
# total financed emissions (absolute, in kg CO2)
total_financed_emissions <- sum(data_subset$mortgage_emissions, na.rm =TRUE)

# Mean emission intensity (t CO2eq / Mio. CHF invested)
mean_emission_intensity_per_investment <- (sum(data_subset$mortgage_emissions, na.rm = TRUE)/1000)/(total_mortgage_value/100000)
print(mean_emission_intensity_per_investment)
# Mean emission intensity (kg CO2/ m2)
mean_emission_intensity_per_m2 <- mean(data_subset$emissions_per_area, na.rm = TRUE)
print(mean_emission_intensity_per_m2)

# mean bank share of mortgage on total assets
mean_bank_share <- total_mortgage_value / total_asset_value
print(mean_bank_share)

# ANALYSIS BY MORTGAGE/ BUILDING TYPE #############################################
distinct_usage_types <- data_subset %>%
  distinct(`Usage de l'immeuble`)

print(distinct_usage_types)


## AND

distinct_building_types <- data_subset %>%
  distinct(`Type d'immeuble`)

print(distinct_building_types)

## data_subset <- data_subset %>%
  mutate(mortgage_type = case_when(
    `Type d'immeuble` %in% c("Agriculture", "Industrie, grands commerces", "Commercial", "Constructions multifonctionnelles, petits commerces", "Habitation à rendement","Hôtel, restaurant, pension") ~ "Commercial",
    `Type d'immeuble` %in% c("PPE Habitation" ) ~ "Residential",
    TRUE ~ "Other"  # In case there are other values you want to handle separately
  ))

data_subset <- data_subset %>%
  mutate(mortgage_type = if_else(`Habitation` == "1", "Residential", "Commercial"))

####### KPI SUMMARY grouped by different variables
## mortgage_type, Type d'immeuble, Usage de l'immeuble, GKLAS, sia_energy_carrier
type_summary <- data_subset %>%
  group_by(mortgage_type) %>%
  summarise(
    mean_emissions = mean(emissions_per_area, na.rm = TRUE),
    median_emissions = median(emissions_per_area, na.rm = TRUE),
    mean_emission_intensity_per_investment = sum(mortgage_emissions, na.rm = TRUE)/1000/(total_mortgage_value/100000),
    sum_financed_emissions = sum(asset_emissions, na.rm = TRUE),
    sum_asset_energetic_area = sum(asset_energetic_area, na.rm = TRUE),
    share_mortgage_value = (sum(mortgage_value, na.rm = TRUE)/ total_mortgage_value),
    count = n()
  )
print(type_summary)
kable(type_summary, caption = "Emission Summary by Building Type")



# CALCULATE COVERAGES #############################################
## comparing the number of non-missing values for each variable to the total number of rows
# Addresses available = proportion of addresses for which we have valid data.
total_rows <- nrow(data_subset)
print(total_rows)

# Coverage ratio for EGID (non-missing EGID)
coverage_EGID <- sum(!is.na(data_subset$EGID)) / total_rows
print(coverage_EGID)

# Coverage ratio for emisisons_per_area
coverage_mortgage_emissions <- sum(!is.na(data_subset$mortgage_emissions)) / total_rows
print(coverage_mortgage_emissions)

# Coverage ratio for financed emissions relative to investments (%)
# where do I have data for mortage emissions, * mortgage_value / total_mortgage_value
data_subset <- data_subset %>%
  mutate(
    emissions_proportion = ifelse(is.na(mortgage_emissions),
                                  mortgage_value,
                                  NA)
  )
coverage_financed_emissions <- sum(data_subset$emissions_proportion, na.rm = TRUE) / total_mortgage_value
print(1-coverage_financed_emissions)

## Whereas the code can get results for 70% of the rows, it represents 72% of total mortgage value.


# OR Coverage ratio related to mortgage value
# Sum of mortgage values when financed emissions is not NA / Total mortgages
coverage_ratio_mortgage <- data_subset %>%
  summarise(
    sum_mortgage_condition = sum(mortgage_value[!is.na(mortgage_emissions)], na.rm = TRUE),
    sum_mortgage = sum(mortgage_value, na.rm = TRUE)
  ) %>%
  mutate(ratio = sum_mortgage_condition / sum_mortgage)

print(coverage_ratio_mortgage)

# Coverage ratio related to area
# Sum of energy relevant area when financed emissions is not NA / Total energy relevant area
coverage_ratio_area <- data_subset %>%
  summarise(
    sum_area_condition = sum(asset_energetic_area[!is.na(mortgage_emissions)], na.rm = TRUE),
    sum_area = sum(asset_energetic_area, na.rm = TRUE)
  ) %>%
  mutate(ratio = sum_area_condition / sum_area)

print(coverage_ratio_area)


# PLOTS ############################################
ggplot(data = data_subset, aes(x = sia_year)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Year Buildings Were Built",
       x = "Year Built",
       y = "Count of Buildings") +
  scale_x_continuous(breaks = seq(1900, 2030, by = 10), limits = c(1900,2030)) +
  scale_y_continuous(breaks = seq(0, 35000, by = 1000)) + # Adjust breaks as needed
  theme_minimal()

ggplot(data = data_subset, aes(x = emissions_per_area)) +
  geom_histogram(binwidth = 10, fill = "red", color = "black") +
  labs(title = "Distribution of emissions per area",
       x = "Emissions per area",
       y = "Count of assets") +
  scale_x_continuous(breaks = seq(0, 400, by = 10)) +
  scale_y_continuous(breaks = seq(0, 35000, by = 1000)) + # Adjust breaks as needed
  theme_minimal()

data_subset$energy_carrier[data_subset$sia_energy_carrier == "4"] <- NA ##
energy_carrier_counts <- table(data_subset$sia_energy_carrier)
custom_names <- c("Gas","Oil","Other")

pie(energy_carrier_counts,
    labels = paste(custom_names, round(prop.table(energy_carrier_counts) * 100, 1), "%"),
    col = c("blue", "green", "red"),
    main = "Share of Energy Carriers")


# CLEAN UP #################################################

# Clear environment
rm(list = ls())
