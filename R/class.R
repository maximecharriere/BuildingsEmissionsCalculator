#' Building Class
#'
#' An S4 class to represent building information, including various attributes
#' like street name, year of construction, energy usage, and more.
#'
#' @slot DEINR numeric The building entrance number.
#' @slot STRNAME character The street name.
#' @slot DPLZ4 numeric The postal code.
#' @slot EGID numeric The unique building identifier.
#' @slot GKAT numeric The building category.
#' @slot GKLAS numeric The building class.
#' @slot GBAUJ numeric The year of construction.
#' @slot GBAUP numeric The period of construction.
#' @slot GABBJ numeric The year of demolition.
#' @slot GAREA numeric The surface area of the building.
#' @slot GASTW numeric The number of floors.
#' @slot GEBF numeric The energy relevant surface.
#' @slot GWAERZH1 character The heat generator for heating 1.
#' @slot GWAERZH2 character The heat generator for heating 2.
#' @slot GENH1 character The energy source for heating 1.
#' @slot GENH2 character The energy source for heating 2.
#' @slot GWAERSCEH1 character The information source for heating 1.
#' @slot GWAERSCEH2 character The information source for heating 2.
#' @slot GWAERDATH1 Date The revision date for heating 1.
#' @slot GWAERDATH2 Date The revision date for heating 2.
#' @slot GWAERZW1 character The heat generator for warm water 1.
#' @slot GWAERZW2 character The heat generator for warm water 2.
#' @slot GENW1 character The energy source for warm water 1.
#' @slot GENW2 character The energy source for warm water 2.
#' @slot GWAERSCEW1 character The information source for warm water 1.
#' @slot GWAERSCEW2 character The information source for warm water 2.
#' @slot GWAERDATW1 Date The revision date for warm water 1.
#' @slot GWAERDATW2 Date The revision date for warm water 2.
#' @slot utilisation_key numeric The utilisation key according to SIA 380/1.
#' @slot climate_code numeric The climate code of the closest climate station.
#' @slot energy_carrier character The energy carrier according to SIA 380/1.
#' @slot walls_refurb_year numeric The year of the last refurbishment of the walls.
#' @slot roof_refurb_year numeric The year of the last refurbishment of the roof.
#' @slot windows_refurb_year numeric The year of the last refurbishment of the windows.
#' @slot floor_refurb_year numeric The year of the last refurbishment of the floor.
#' @slot heating_install_year numeric The year of the installation of the heating system.
#' @slot heatEnergy numeric Annual heat energy required per area, in MJ/m2 per year.
#' @slot emissionCoefficient numeric Applied CO2 emission coefficient according to the given energy carrier, in kg/MJ.
#' @slot emissionsPerArea numeric Annual CO2 emissions per area, in kg/m2 per year.
#' @slot emissionsTotal numeric Total annual CO2 emissions, in kg per year.
#' @slot error_comments character A string containing error messages.
#' @export
setClass("Building",
  slots = list(
    DEINR = "numeric",
    STRNAME = "character",
    DPLZ4 = "numeric",
    EGID = "numeric",
    GKAT = "numeric",
    GKLAS = "numeric",
    GBAUJ = "numeric",
    GBAUP = "numeric",
    GABBJ = "numeric",
    GAREA = "numeric",
    GASTW = "numeric",
    GEBF = "numeric",
    GWAERZH1 = "numeric",
    GWAERZH2 = "numeric",
    GENH1 = "numeric",
    GENH2 = "numeric",
    GWAERSCEH1 = "numeric",
    GWAERSCEH2 = "numeric",
    GWAERDATH1 = "Date",
    GWAERDATH2 = "Date",
    GWAERZW1 = "numeric",
    GWAERZW2 = "numeric",
    GENW1 = "numeric",
    GENW2 = "numeric",
    GWAERSCEW1 = "numeric",
    GWAERSCEW2 = "numeric",
    GWAERDATW1 = "Date",
    GWAERDATW2 = "Date",
    utilisation_key = "numeric",
    climate_code = "character",
    energy_carrier = "character",
    walls_refurb_year = "numeric",
    roof_refurb_year = "numeric",
    windows_refurb_year = "numeric",
    floor_refurb_year = "numeric",
    heating_install_year = "numeric",
    heatEnergy = "numeric",
    emissionCoefficient = "numeric",
    emissionsPerArea = "numeric",
    emissionsTotal = "numeric",
    error_comments = "character"
  ),
  prototype = list(
    DEINR = NA_real_,
    STRNAME = NA_character_,
    DPLZ4 = NA_real_,
    EGID = NA_real_,
    GKAT = NA_real_,
    GKLAS = NA_real_,
    GBAUJ = NA_real_,
    GBAUP = NA_real_,
    GABBJ = NA_real_,
    GAREA = NA_real_,
    GASTW = NA_real_,
    GEBF = NA_real_,
    GWAERZH1 = NA_real_,
    GWAERZH2 = NA_real_,
    GENH1 = NA_real_,
    GENH2 = NA_real_,
    GWAERSCEH1 = NA_real_,
    GWAERSCEH2 = NA_real_,
    GWAERDATH1 = as.Date(NA),
    GWAERDATH2 = as.Date(NA),
    GWAERZW1 = NA_real_,
    GWAERZW2 = NA_real_,
    GENW1 = NA_real_,
    GENW2 = NA_real_,
    GWAERSCEW1 = NA_real_,
    GWAERSCEW2 = NA_real_,
    GWAERDATW1 = as.Date(NA),
    GWAERDATW2 = as.Date(NA),
    utilisation_key = NA_real_,
    climate_code = NA_character_,
    energy_carrier = NA_character_,
    walls_refurb_year = NA_real_,
    roof_refurb_year = NA_real_,
    windows_refurb_year = NA_real_,
    floor_refurb_year = NA_real_,
    heating_install_year = NA_real_,
    heatEnergy = NA_real_,
    emissionCoefficient = NA_real_,
    emissionsPerArea = NA_real_,
    emissionsTotal = NA_real_,
    error_comments = NA_character_
  )
)
