# TODO 2nd heating and 2nd hot water system not taken into account

fill_building_data <- function(building) {
  tryCatch(
    {
      # Call the regbl API to get the building data
      building <- request_regbl(building)

      ## Prepare the building data for the co2calculator
      # If the energy reference surface is not available, calculate it from the gross area times the number of floors.
      if (!is.na(building$GEBF)) {
        building$energy_relevant_area <- building$GEBF
      } else {
        building$energy_relevant_area <- building$GAREA * building$GASTW
      }
      # If the year of construction is not available in RegBl,
      # use the smallest year of the period of construction given by RegBl.
      # If the period of construction is not available in RegBl, use 1900.
      if (!is.na(building$GBAUJ)) {
        building$year <- building$GBAUJ
      } else {
        building$year <- .constants$constructionYear_RegblPeriod2Year[[as.character(building$GBAUP)]]
        if (is.na(building$year)) {
          building$year <- 1900
        }
      }
      # Find the closest climate station in the 40 listed in SIA 2028:2010 https://www.sia.ch/fileadmin/content/download/sia-norm/korrigenda_sn/2028-C1_2015_d.pdf
      building$climate_code <- find_closest_station(co2calculatorPACTA2022::climate, building)
      # Convert the building class from RegBl to the SIA 380/1 standard
      if (building$GKLAS %in% names(.constants$buildingClass_Regbl2Sia)) {
        building$utilisation_key <- .constants$buildingClass_Regbl2Sia[[as.character(building$GKLAS)]]
      } else if (is.na(building$GKLAS)) {
        stop("Building class (GKLAS) from RegBl unknown.")
      } else {
        stop("Building class (GKLAS) from RegBl (with ID ", building$GKLAS, ") not found in the LUT '.constants$buildingClass_Regbl2Sia'. Please update the LUT.")
      }
      # Convert the energy carrier type from RegBl to the SIA 380/1 standard
      if (building$GENH1 %in% names(.constants$energyCarrier_Regbl2Sia)) {
        building$energy_carrier <- .constants$energyCarrier_Regbl2Sia[[as.character(building$GENH1)]]
      } else {
        building$energy_carrier <- "other"
      }
      # Number of floors
      building$floors <- building$GASTW
      # The year of the installation of the heating system
      building$heating_install_year <- as.numeric(substr(building$GWAERDATH1, 1, 4))

      # If the building is demolished, stop the process
      if (!is.na(building$GABBJ)) {
        stop("Building was demolished in ", building$GABBJ, ".")
      }

      # Calculate the CO2 emissions
      tryCatch(
        {
          emissions <- co2calculatorPACTA2022::calculate_emissions(
            area = building$energy_relevant_area,
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
          building[names(emissions)] <- emissions
        },
        error = function(e) {
          if(.constants$saveLogs){
            save(building, file = paste0("log/error_building_", building$EGID, ".RData"))
          }
          stop("from co2calculator: ", e$message)
        }
      )

      return(building)
    },
    error = function(e) {
      building$error_comments <- append_error_message(building$error_comments, e$message)
      return(building)
    }
  )
}
