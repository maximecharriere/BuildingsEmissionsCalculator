# TODO 2nd heating and 2nd hot water system not taken into account

fill_building_data <- function(building) {
  tryCatch(
    {
      # Call the regbl API to get the building data
      building <- request_regbl(building)

      # If the building is demolished, stop the process
      if (!is.na(building$GABBJ)) {
        stop("Building was demolished in ", building$GABBJ, ".")
      }

      # Convert the building data from the regbl format to the sia format
      building <- regbl_2_sia_converter(building)

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
