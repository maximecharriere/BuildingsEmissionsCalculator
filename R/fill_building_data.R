#' Retrieve and Enhance Building Data with RegBl data and CO2 Emissions Calculations
#'
#' This function enriches a given building's data record by fetching additional details from the RegBl API,
#' converting the data to the SIA format used by the co2calculatorPACTA2022 package, and computing the building's CO2 emissions. If the building
#' has been demolished, as indicated by a non-NA `GABBJ` value, the function halts and reports the demolition.
#' Otherwise, it proceeds to convert the format of the building data and calculate emissions using various
#' building attributes.
#'
#' @param building A list or data frame containing at least minimal identifying information for a building,
#'                 which is then used to fetch and augment data from the RegBl database.
#' @param data_source A character string specifying the source of the RegBl data, either 'sqlite' or 'web'.
#'
#' @return An enhanced version of the input `building` data, including additional details and computed CO2 emissions.
#'
#' @examples
#' building_data <- list(EGID = "123456")
#' enriched_building_data <- fill_building_data(building_data)
fill_building_data <- function(building, sqlite_conn = NULL) {
  building <- tryCatch(
    {
      library(co2calculatorPACTA2022) # TODO found why the script is not working if I remove this line

      # Split the address into components if the address is not already split
      if (is.na(building$STRNAME) && is.na(building$DEINR) && !is.na(building$ADDRESS)) {
        building <- split_address(building)
      }

      # identify the building 
      building <- egid_search(building, sqlite_conn)

      # Request regbl to get the building data
      building <- request_building_data(building, sqlite_conn = sqlite_conn)

      # If the building is demolished, stop the process
      if (!(is.na(building$GABBJ))) {
        stop("Building was demolished in ", building$GABBJ, ".")
      }

      # Compute the energetic area of the asset
      building$asset_energetic_area <- get_asset_energetic_area(building)

      # Convert the building data from the regbl format to the sia format
      building <- regbl_2_sia_converter(building)

      # Calculate the CO2 emissions
      tryCatch(
        {
          result <- co2calculatorPACTA2022::calculate_emissions(
            area = building$asset_energetic_area,
            floors = building$floors,
            year = building$year,
            utilisation_key = building$utilisation_key,
            climate_code = building$climate_code,
            energy_carrier = building$energy_carrier,
            walls_refurb_year = building$walls_refurb_year,
            roof_refurb_year = building$roof_refurb_year,
            windows_refurb_year = building$windows_refurb_year,
            floor_refurb_year = building$floor_refurb_year,
            heating_install_year = building$heating_install_year
          )
          # Update the emissions to the building data
          building$heat_energy <- result$heatEnergy
          building$emission_coefficient <- result$emissionCoefficient
          building$emissions_per_area <- result$emissionsPerArea
          building$asset_emissions <- result$emissionsTotal
        },
        error = function(e) {
          stop("Error from co2calculator: ", e$message)
        }
      )
      return(building)
    },
    error = function(e) {
      building$error_comments <- append_error_message(building$error_comments, e$message)
      message("Error on EGID ", building$EGID, ": ", e$message)
      return(building)
    }
  )
  return(building)
}

#' Fill missing data in the buildings dataframe with RegBl data and computed CO2 emissions
#'
#' This function processes a dataframe of buildings, applying the `fill_building_data` function to each
#' row/building in parallel. It ensures that the input dataframe contains all necessary columns, filling
#' any missing ones with NA values, then computes detailed building information and CO2 emissions for each
#' building. The function leverages parallel computing capabilities to enhance performance and efficiency.
#'
#' @param buildings_df A dataframe where each row represents a building and contains minimal data needed
#'        for enhancement, such as an identifier used to fetch additional data.
#'
#' @return A new dataframe with the same structure as `buildings_df` but augmented with detailed building
#'         data and CO2 emissions computations for each building. If any data cannot be retrieved or calculated,
#'         the corresponding building will have its `error_comments` field updated with the error message.
#'
#' @examples
#' buildings_df <- data.frame(EGID = c("123456", "789012"))
#' enhanced_buildings_df <- fill_buildings_df(buildings_df)
#'
#' @details
#' The function first standardizes the input dataframe to ensure it includes all necessary columns,
#' leveraging the `standardize_buildings_df` function. Then, it uses the `future.apply::future_lapply`
#' function to apply `fill_building_data` to each building in the dataframe in parallel, improving
#' processing time on multicore systems. Each building's data is individually retrieved and processed,
#' with results aggregated into a single dataframe returned at the end.
#'
#' The parallel execution model can be adjusted based on system resources by changing the number of
#' workers in the `future::plan` call.
#' @export
fill_buildings_df <- function(buildings_df, regbl_db_path = NULL, log_file = "log/log.txt") {
  progressr::handlers(global = TRUE)
  progressr::handlers("cli")

  # Open log file for writing
  cat("Logging parallel function outputs...\n", file = log_file)

  # Add missing columns to the dataframe, filled with NA values
  buildings_df <- standardize_buildings_df(buildings_df, names(.constants$buildings_df_columns)) #TODO with the new handling of Excel tables, this function does not work anymore. If extra columns are added, it's not possible to past the table in the Excel sheet.

  # Initialize a progress bar with the total number of iterations
  p <- progressr::progressor(nrow(buildings_df))

  # Open the SQLite database connection
  sqlite_conn <- NULL
  if (is.null(regbl_db_path)) {
    stop("RegBl Database filepath not provided.")
  }
  sqlite_conn <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = regbl_db_path)
  on.exit(RSQLite::dbDisconnect(sqlite_conn), add = TRUE) # Ensure the connection is closed

  # Set SQLite database parameters for performance optimization
  RSQLite::dbExecute(sqlite_conn, "PRAGMA synchronous = OFF;")
  RSQLite::dbExecute(sqlite_conn, "PRAGMA journal_mode = WAL;")
  RSQLite::dbExecute(sqlite_conn, "PRAGMA cache_size = -1000000;") # 1GB of memory used for cache
  RSQLite::dbExecute(sqlite_conn, "PRAGMA temp_store = MEMORY;")


  # Call fill_building_data on each building
  multithreading <- FALSE
  if (multithreading) {
    stop("Multithreading is not yet tested")
    # Set up parallelization plan (e.g., multisession to use multiple cores)
    future::plan(future::multisession, workers = 4)
    buildings <- future.apply::future_lapply(seq_len(nrow(buildings_df)), function(i) {
      building <- fill_building_data(buildings_df[i, ], sqlite_conn = sqlite_conn)
      p()
      return(building)
    })
  } else {
    buildings <- list()
    for (i in seq_len(nrow(buildings_df))) {
      building <- fill_building_data(buildings_df[i, ], sqlite_conn = sqlite_conn)
      p()
      buildings[[i]] <- building
    }
  }

  # Combine the results back into the original dataframe
  # Assuming results are returned as dataframes or named lists that can be rbinded
  buildings_df_new <- do.call(rbind, buildings)

  return(buildings_df_new)
}

get_financed_share <- function(building) {
  # check that the total number of dwellings is available in RegBl, and that the bank gave a number of financed dwellings
  if (is.na(building$GANZWHG) || is.na(building$ewid_count)){
    return(1.0) # if the number of financed dwellings is not available, assume that the bank financed the whole building
  }
  return(min(building$ewid_count / building$GANZWHG, 1.0)) # return the share of financed dwellings, capped at 100%
}

get_building_area <- function(building) {
  if (is.na(building$GAREA) || is.na(building$GASTW)) {
    stop("The Building cannot be processed because the ground area or the number of floors is missing in RegBl.")
  }
  return(building$GAREA * building$GASTW)
}

get_asset_energetic_area <- function(building) {
  if (!is.na(building$financed_area)) {
    return(building$financed_area * .constants$energeticAreaFactor)
  } else if (!is.na(building$GEBF)) {
    return(building$GEBF * get_financed_share(building))
  } else {
    financed_area <- get_building_area(building) * get_financed_share(building)
    return(financed_area * .constants$energeticAreaFactor)
  }
}
