# TODO 2nd heating and 2nd hot water system not taken into account

fill_building_data <- function(building) {
  tryCatch(
    {
      # Call the regbl API to get the building data
      building <- request_regbl(building)

      ## Prepare the building data for the co2calculator
      # If the energy reference surface is not available, calculate it from the gross area times the number of floors.
      if (is.na(building$GEBF)) {
        building$GEBF <- building$GAREA * building$GASTW
      }
      # If the year of construction is not available in RegBl,
      # use the smallest year of the period of construction given by RegBl.
      # If the period of construction is not available in RegBl, use 1900.
      if (is.na(building$GBAUJ)) {
        building$GBAUJ <- .constants$constructionYear_RegblPeriod2Year[[as.character(building$GBAUP)]]
        if (is.na(building$GBAUJ)) {
          building$GBAUJ <- 1900
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

      # Calculate the CO2 emissions
      tryCatch(
        {
          emissions <- co2calculatorPACTA2022::calculate_emissions(
            area = building$GEBF,
            floors = building$GASTW,
            year = building$GBAUJ,
            utilisation_key = building$utilisation_key,
            climate_code = building$climate_code,
            energy_carrier = building$energy_carrier
          )
        },
        error = function(e) {
          save(building, file = paste0("error_building_", building$EGID, ".RData"))
          stop("from co2calculator: ", e$message)
        }
      )



      # Update the emissions to the building data
      building[names(emissions)] <- emissions

      return(building)
    },
    error = function(e) {
      building$error_comments <- append_error_message(building$error_comments, e$message)
      return(building)
    }
  )
}
