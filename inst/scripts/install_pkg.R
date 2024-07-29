# Author: Maxime Charriere, contact@maximecharriere.ch
# Date: 02.2024
# This script is used to install dependencies for the package.
# Used during the dev phase.

# --------------------- #
# ----- FUNCTIONS ----- #
# --------------------- #

#' pkg_install
#'
#' Install one or more packages from a given CRAN repository,
#' if they're not already installed. Optionally, force update
#' if a newer version exists.
#'
#' @param packages Vector of packages to install.
#' @param repos Link to the CRAN repo to get the packages.
#' @param force_update Logical, if TRUE, reinstall packages if newer versions are available.
#'
#' @examples
#' pkg_install(packages = c("knitr", "readxl"), repos = "https://stat.ethz.ch/CRAN", force_update = TRUE)
pkg_install <- function(packages, repos = "https://stat.ethz.ch/CRAN", force_update = FALSE) {
  installed <- utils::installed.packages()
  available <- utils::available.packages(repos = repos)

  # Determine packages to install or update
  packagestoinstall <- sapply(packages, function(pkg) {
    if (pkg %in% rownames(installed)) {
      installed_version <- as.character(installed[pkg, "Version"])
      available_version <- as.character(available[pkg, "Version"])

      # Check if we need to update the package
      if (force_update && available_version > installed_version) {
        return(pkg)
      } else {
        return(NA)
      }
    } else {
      return(pkg)
    }
  })

  # Filter out NAs
  packagestoinstall <- na.omit(packagestoinstall)

  # Install or update packages
  if (length(packagestoinstall) > 0L) {
    utils::install.packages(packagestoinstall, repos = repos)
  } else {
    print("All requested packages are already installed and up to date.")
  }
}

# --------------------- #
# ------- MAIN -------- #
# --------------------- #

pkg_install(packages = c(
  "tidyverse", # Data manipulation and visualization
  "languageserver", # Language server for R, provides IDE features
  "writexl", # Write data frames to Excel files
  "readxl", # Read Excel files
  "jsonlite", # Read and write JSON data
  "roxygen2", # Generate documentation from inline comments
  "pkgdown", # Generate a website for your package
  "devtools", # Tools to make package development easier
  "usethis", # Automate package and project setup
  "RSQLite", # Interface to SQLite databases
  "httr2", # Modern HTTP client for R
  "xml2", # Work with XML files
  "ids", # Generate unique identifiers
  "future", # Support for concurrent and parallel R programming
  "future.apply", # Apply functions in parallel using 'future'
  "knitr", # Dynamic report generation in R
  "htmltools", # Tools for HTML generation and rendering
  "progressr", # Progress updates in the R console
  "openxlsx2" # Read and write Excel files
), repos = "https://stat.ethz.ch/CRAN", force_update = TRUE)
