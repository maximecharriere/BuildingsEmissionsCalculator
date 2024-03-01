library(co2calculatorPACTA2022)
library(future.apply)
library(future)

fill_dataframe <- function(buildings_df) {
  # Add missing columns to the dataframe, filled with NA values
  buildings_df <- add_missing_columns(buildings_df)

  ## Execute fill_building_data on each row of the dataframe
  # Set up parallelization plan (e.g., multisession to use multiple cores)
  plan(multisession, workers = 1) # Adjust the number of workers based on your machine's capabilities

  # Use future_lapply to apply the function to each row of the dataframe
  results <- future_lapply(seq_len(nrow(buildings_df)), function(i) fill_building_data(buildings_df[i, ]))

  # Combine the results back into the original dataframe
  # Assuming results are returned as dataframes or named lists that can be rbinded
  buildings_df_new <- do.call(rbind, results)

  # Bind the additional information to the original dataframe
  # buildings_df <- cbind(buildings_df, additional_info_df)

  return(buildings_df_new)
}
