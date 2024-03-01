# Author: Maxime Charriere, contact@maximecharriere.ch
# Date: 02.2024
# This script is used to build, install, and document the package.
# Used during the dev phase.

# First install pandoc https://github.com/jgm/pandoc/releases
devtools::document() # Equivalent to roxygenize(), also updates documents
devtools::build() # Build the package
devtools::install() # Install the package to your R library
pkgdown::build_site() # Build the doc website
