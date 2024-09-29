#' Convert Building Data from RegBl to SIA Format
#'
#' This function converts building data retrieved from the RegBl database into the format
#' required by the SIA (Swiss Society of Engineers and Architects) standards, specifically
#' for use with the co2calculatorPACTA2022 package
#' (see \url{https://maximecharriere.github.io/co2calculatorPACTA2022/}).
#'
#' @param building A named list or dataframe containing building data in RegBl format.
#'
#' @return The same building data structure but with additional values converted to the SIA format.
#'
#' @details
#' The conversion rules applied by this function include:
#' - The energy relevant area is taken from `GEBF` if available, or calculated from `GAREA` times `GASTW`.
#' - The construction year is taken from `GBAUJ` if available, or the earliest year given by `GBAUP`.
#' - The closest climate station code is determined for the given building location.
#' - The utilisation key is mapped from the RegBl building class (`GKLAS`) using a lookup table.
#' - The energy carrier is mapped from the RegBl energy source type (`GENH1`) using a lookup table.
#' - The number of floors (`floors`) and the year the heating system was installed
#'   (`heating_install_year`) are directly assigned.
regbl_2_sia_converter <- function(building) {
  # Convert the building data from the regbl format to the sia format
  # The sia format is used by the co2calculatorPACTA2022 package

  # If the year of construction is not available in RegBl,
  # use the smallest year of the period of construction given by RegBl.
  # If the period of construction is not available in RegBl, use 1900.
  if (!is.na(building$GBAUJ)) {
    building$sia_year <- building$GBAUJ
  } else {
    building$sia_year <- .constants$constructionYear_RegblPeriod2Year[[as.character(building$GBAUP)]]
    if (is.na(building$sia_year)) {
      building$sia_year <- 1900
    }
  }
  # Find the closest climate station in the 40 listed in SIA 2028:2010 https://www.sia.ch/fileadmin/content/download/sia-norm/korrigenda_sn/2028-C1_2015_d.pdf
  building$sia_climate_code <- find_closest_station(climate, building)
  # Convert the building class from RegBl to the SIA 380/1 standard
  if (building$GKLAS %in% names(.constants$buildingClass_Regbl2Sia)) {
    building$sia_utilisation_key <- .constants$buildingClass_Regbl2Sia[[as.character(building$GKLAS)]]
  } else if (is.na(building$GKLAS)) {
    stop("No Building class (GKLAS) found on RegBl database.")
  } else {
    stop("Building class (GKLAS) from RegBl (with ID ", building$GKLAS, ") not found in the LUT '.constants$buildingClass_Regbl2Sia'. Please update the LUT.")
  }
  # Convert the energy carrier type from RegBl to the SIA 380/1 standard
  if (building$GENH1 %in% names(.constants$energyCarrier_Regbl2Sia)) {
    building$sia_energy_carrier <- .constants$energyCarrier_Regbl2Sia[[as.character(building$GENH1)]]
  } else {
    building$sia_energy_carrier <- "other"
  }
  # Energy relevant area
  building$sia_area <- building$GEBF
  # Number of floors
  building$sia_floors <- building$GASTW
  # The year of the installation of the heating system
  building$sia_heating_install_year <- as.numeric(substr(building$GWAERDATH1, 1, 4))

  return(building)
}
