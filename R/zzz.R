# Author: Maxime Charriere, contact@maximecharriere.ch
# Date: 02.2024
# This script contain the code to execute at the loading or unloading of the package.

.onLoad <- function(libname, pkgname) {
  # Path to the json file within the package
  constants_path <- system.file("extdata", "constants.json", package = pkgname)
  # Load the JSON file using jsonlite
  constants <- jsonlite::read_json(constants_path)
  # Path to the json file within the package
  utilisation_path <- system.file("extdata", "utilisation.json", package = pkgname)
  # Load the JSON file using jsonlite
  utilisation <- jsonlite::read_json(utilisation_path)
  # Path to the json file within the package
  # climate_path <- system.file("extdata", "climate.json", package = pkgname)
  # # Load the JSON file using jsonlite
  # climate <- jsonlite::read_json(climate_path)

  list2env(list(".constants" = constants, ".utilisation" = utilisation), envir = asNamespace(pkgname))
}


