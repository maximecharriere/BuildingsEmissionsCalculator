# Author:   Annika Schmidt
# Content: Financed emissions mortgages BCJ


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
data <- read.xlsx("20240904_BCJ.xlsx", startRow = 4, colNames = TRUE)
head(data)
data$bank_share <- as.numeric(data$bank_share)
typeof(data$bank_share)
data_subset <- data[1:2044, ]
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

#### Check
data_subset <- data_subset %>%
  mutate(
    check_equal = ifelse(!is.na(emissions_per_area) & !is.na(asset_emissions) & !is.na(energy_relevant_area),
                         emissions_per_area == (asset_emissions / energy_relevant_area),
                         NA)
  )

summary(data_subset$check_equal)

### Check for outliers
# Boxplots
#It displays the distribution of data based on quartiles and highlights outliers as points outside the whiskers.
ggplot(data_subset, aes(y = emissions_per_area)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Boxplot with Outliers", y = "Emmissions per area") +
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

# Remove observations where 'emissions_per_area' is greater than 350
data_subset <- data_subset %>%
  filter(emissions_per_area <= 350 | is.na(emissions_per_area))

# Calculate emissions ############################################
data_subset$financed_emissions <- data_subset$asset_emissions*data_subset$bank_share
data_subset$financed_area <- data_subset$asset_emissions * data_subset$bank_share*data_subset$energy_relevant_area

# Create values ############################################

# sum of asset value
total_asset_value <- sum(data_subset$asset_value, na.rm = TRUE)
# sum of mortgage value
total_mortgage_value <- sum(data_subset$mortgage_value, na.rm = TRUE)
# sum of financed emissions (absolute)
total_financed_emmissions <- sum(data_subset$financed_emissions, na.rm =TRUE)

# emission intensity t CO2eq / Mio. CHF invested
emission_intensity_per_investment <- (sum(data_subset$financed_emissions, na.rm = TRUE)/1000)/(total_mortgage_value/100000)
print(emission_intensity_per_investment)

# bank share of mortgage on total assets
total_bank_share <- total_mortgage_value / total_asset_value


# ANALYSIS BY MORTGAGE/ BUILDING TYPE #############################################
distinct_building_types <- data_subset %>%
  distinct(Type.de.bien)
print(distinct_building_types)

data_subset <- data_subset %>%
  mutate(mortgage_type = case_when(
    Type.de.bien %in% c("Agriculture", "Autre couverture hypothècaire", "Industrie, grands commerces", "Commercial", "Constructions multifonctionnelles, petits commerces", "Habitation à rendement","Hôtel, restaurant, pension") ~ "Commercial",
    Type.de.bien %in% c("PPE Habitation") ~ "Residential",
    TRUE ~ "Other"  # In case there are other values you want to handle separately
  ))


####### MORTGAGE TYPE SUMMARY
mortgage_type_summary <- data_subset %>%
  group_by(mortgage_type) %>%
  summarise(
    mean_emissions = mean(emissions_per_area, na.rm = TRUE),
    median_emissions = median(emissions_per_area, na.rm = TRUE),
    max_emissions = max(emissions_per_area, na.rm = TRUE),
    sum_financed_emissions = sum(financed_emissions, na.rm = TRUE),
    sum_financed_area = sum(financed_area, na.rm = TRUE),
    share_mortgage_value = (sum(mortgage_value, na.rm = TRUE)/ total_mortgage_value),
    count = n()
  )
print(mortgage_type_summary)
kable(mortgage_type_summary, caption = "Emission Summary by Building Type")


######
emission_summary <- data_subset %>%
  group_by(Type.de.bien) %>%
  summarise(
    mean_emissions = mean(emissions_per_area, na.rm = TRUE),
    median_emissions = median(emissions_per_area, na.rm = TRUE),
    max_emissions = max(emissions_per_area, na.rm = TRUE),
    sum_financed_emissions = sum(financed_emissions, na.rm = TRUE),
    sum_financed_area = sum(financed_area, na.rm = TRUE),
    share_mortgage_value = (sum(mortgage_value, na.rm = TRUE)/ total_mortgage_value),
    count = n()
  )
print(emission_summary)
kable(emission_summary, caption = "Emission Summary by Building Type")

# CALCULATE COVERAGES #############################################
## comparing the number of non-missing values for each variable to the total number of rows
# Addresses available = proportion of addresses for which we have valid data.
total_rows <- nrow(data_subset)

# Coverage ratio for addresses (non-missing addresses)
coverage_addresses <- sum(!is.na(data_subset$ADDRESS)) / total_rows
print(coverage_addresses)

# Coverage ratio for EGID (non-missing EGID)
coverage_EGID <- sum(!is.na(data_subset$EGID)) / total_rows
print(coverage_EGID)

# Coverage ratio for emisisons_per_area
coverage_emissions_per_area <- sum(!is.na(data_subset$emissions_per_area)) / total_rows
print(coverage_emissions_per_area)

# Coverage ratio for financed emissions
coverage_financed_emissions <- sum(!is.na(data_subset$financed_emissions)) / total_rows
print(coverage_financed_emissions)

# Coverage ratio related to mortgage value
# Sum of mortgage values when financed emissions is not NA / Total mortgages
coverage_ratio_mortgage <- data_subset %>%
  summarise(
    sum_mortgage_condition = sum(mortgage_value[!is.na(financed_emissions)], na.rm = TRUE),
    sum_mortgage = sum(mortgage_value, na.rm = TRUE)
  ) %>%
  mutate(ratio = sum_mortgage_condition / sum_mortgage)

print(coverage_ratio_mortgage)

# Coverage ratio related to area
# Sum of energy relevant area when financed emissions is not NA / Total energy relevant area
coverage_ratio_area <- data_subset %>%
  summarise(
    sum_area_condition = sum(energy_relevant_area[!is.na(financed_emissions)], na.rm = TRUE),
    sum_area = sum(energy_relevant_area, na.rm = TRUE)
  ) %>%
  mutate(ratio = sum_area_condition / sum_area)

print(coverage_ratio_area)


# PLOTS ############################################
ggplot(data = data_subset, aes(x = year)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Year Buildings Were Built",
       x = "Year Built",
       y = "Count of Buildings") +
  scale_x_continuous(breaks = seq(1700, 2030, by = 20)) +
  scale_y_continuous(breaks = seq(0, 400, by = 50)) + # Adjust breaks as needed
  theme_minimal()

ggplot(data = data_subset, aes(x = emissions_per_area)) +
  geom_histogram(binwidth = 10, fill = "red", color = "black") +
  labs(title = "Distribution of emissions per area",
       x = "Emissions per area",
       y = "Count of assets") +
   scale_x_continuous(breaks = seq(0, 150, by = 20)) +
   scale_y_continuous(breaks = seq(0, 700, by = 50)) + # Adjust breaks as needed
  theme_minimal()

data_subset$energy_carrier[data_subset$energy_carrier == "4"] <- NA ##
energy_carrier_counts <- table(data_subset$energy_carrier)


pie(energy_carrier_counts,
    labels = paste(names(energy_carrier_counts), round(prop.table(energy_carrier_counts) * 100, 1), "%"),
    col = c("blue", "green", "red"),
    main = "Share of Energy Carriers")


# CLEAN UP #################################################

# Clear environment
rm(list = ls())
