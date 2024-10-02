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
      # library(co2calculatorPACTA2022) # TODO found why the script is not working if I remove this line

      #############
      # Identify the building
      #############
      building <- egid_search(building, sqlite_conn)

      #############
      # Building data
      #############

      # Request regbl to get the building data
      building <- request_regbl_data(building, sqlite_conn = sqlite_conn)

      # If the building is demolished, stop the process
      if (!(is.na(building$GABBJ))) {
        stop("Building was demolished in ", building$GABBJ, ".")
      }

      # Compute the energetic area of the building
      building$GEBF <- get_building_energetic_area(building)

      # Convert the building data from the regbl format to the sia format
      building <- regbl_2_sia_converter(building)

      # Calculate the CO2 emissions of the building
      tryCatch(
        {
          result <- calculate_emissions(
            area = building$sia_area,
            floors = building$sia_floors,
            year = building$sia_year,
            utilisation_key = building$sia_utilisation_key,
            climate_code = building$sia_climate_code,
            energy_carrier = building$sia_energy_carrier,
            walls_refurb_year = building$sia_walls_refurb_year,
            roof_refurb_year = building$sia_roof_refurb_year,
            windows_refurb_year = building$sia_windows_refurb_year,
            floor_refurb_year = building$sia_floor_refurb_year,
            heating_install_year = building$sia_heating_install_year
          )
          # Update the emissions to the building data
          building$heat_energy <- result$heatEnergy
          building$emission_coefficient <- result$emissionCoefficient
          building$emissions_per_area <- result$emissionsPerArea
          building$building_emissions <- result$emissionsTotal
        },
        error = function(e) {
          stop("Error from co2calculator: ", e$message)
        }
      )

      #############
      # Asset data
      #############
      building <- fill_asset_energetic_area(building)
      building$asset_emissions <- get_asset_emissions(building)

      #############
      # Mortgage data
      #############
      building$asset_bank_share <- get_asset_bank_share(building)
      building$mortgage_emissions <- get_mortgage_emissions(building)


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
fill_buildings_df <- function(buildings_df, regbl_db_path = NULL) {
  progressr::handlers(global = TRUE)
  progressr::handlers("cli")

  # Add missing columns to the dataframe, filled with NA values
  buildings_df <- add_missing_columns(buildings_df)
  buildings_df <- convert_columns_types(buildings_df)

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

get_asset_bank_share <- function(building)  {
  if (is.na(building$mortgage_value) || is.na(building$asset_value)) {
    return(1.0) # if the mortgage value or the asset value is not available, assume that the whole asset is financed
  } else if (building$mortgage_value <= 0) {
    return(0.0)
  } else if (building$asset_value <= 0) {
    return(1.0)
  } else if (building$mortgage_value > building$asset_value) {
    return(1.0)
  } else {
    return(building$mortgage_value / building$asset_value)
  }
}

get_asset_building_share <- function(building) {
  # check that the total number of dwellings is available in RegBl, and that the bank gave a number of financed dwellings
  if (is.na(building$GANZWHG) || is.na(building$asset_ewid_count)){
    return(1.0) # if the number of financed dwellings is not available, assume that the whole building is financed
  }
  return(min(building$asset_ewid_count / building$GANZWHG, 1.0)) # return the share of financed dwellings, capped at 100%
}

get_building_area <- function(building) {
  if (is.na(building$GAREA) || is.na(building$GASTW)) {
    stop("The Building cannot be processed because the ground area or the number of floors is missing in RegBl.")
  }
  return(building$GAREA * building$GASTW)
}

get_building_energetic_area <- function(building) {
  if (!is.na(building$GEBF)) {
    return(building$GEBF)
  } else {
    return(get_building_area(building) * .constants$energeticAreaFactor)
  }
}

fill_asset_energetic_area <- function(building) {
  if (!is.na(building$asset_energetic_area)) {
    return(building)
  } else if (!is.na(building$asset_living_area)) {
    building$asset_energetic_area <- building$asset_living_area * .constants$energeticAreaFactor
    return(building)
  } else {
    building$asset_building_share <- get_asset_building_share(building)
    building$GEBF <- get_building_energetic_area(building)
    building$asset_energetic_area <- building$GEBF * building$asset_building_share
    return(building)
  }
}

get_asset_emissions <- function(building) {
  if (!is.na(building$asset_emissions)) {
    return(building$asset_emissions)
  } else if (is.na(building$asset_energetic_area) || is.na(building$emissions_per_area)) {
    stop("The Asset Emissions cannot be processed because the asset energetic area or the emissions per area is missing. Please compute these values first.")
  }
  return(building$asset_energetic_area * building$emissions_per_area)
}

get_mortgage_emissions <- function(building){
  if (!is.na(building$mortgage_emissions)) {
    return(building$mortgage_emissions)
  } else if (is.na(building$asset_emissions) || is.na(building$asset_bank_share)) {
    stop("The Mortgage Emissions cannot be processed because the asset emissions or the bank share in the asset is missing. Please compute these values first.")
  }
  return(building$asset_emissions * building$asset_bank_share)
}
