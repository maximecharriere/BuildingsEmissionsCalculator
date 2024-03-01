# Author: Maxime Charriere, contact@maximecharriere.ch
# Date: 02.2024
# This script contain the code to execute at the loading or unloading of the package.

.onLoad <- function(libname, pkgname) {
  ## Store constant from the extdata/constants.json file in the .constants local environment

  # Create a new environment to store the constants
  .constants <- new.env(parent = emptyenv())
  # Path to the constants.json file within the package
  constants_path <- system.file("extdata", "constants.json", package = pkgname)
  # Load the JSON file using jsonlite
  constants <- jsonlite::fromJSON(constants_path)
  # Assign the loaded constants to the .Constants environment
  list2env(constants, envir = .constants)
  # Assign the .Constants environment to the package namespace
  assign(".constants", .constants, envir = asNamespace(pkgname))
}
