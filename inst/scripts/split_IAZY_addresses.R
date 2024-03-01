# Author: Maxime Charriere, contact@maximecharriere.ch
# Date: 02.2024
# This script is used to split the IAZY addresses and house number, from
# the format "ADDRESS NBR" to "ADDRESS" and "NBR".
# Used during the dev phase.

# --------------------- #
# ----- FUNCTIONS ----- #
# --------------------- #

#' Split Address into Street Name and House Number
#'
#' This function takes a single string address and splits it into two components: the street name and the house number. The street is everything before the last sequence of digits which may include letters (e.g., "54A" or "10A").
#'
#' @param address A single string containing a street name followed by a house number.
#' @return A list with two elements: $STRNAME containing the street name, and $DEINR containing the house number.
split_address <- function(address) {
  # This pattern looks for any sequence of digits (\d+) possibly followed by non-digits (\D*)
  # at the end of the string ($)
  pattern <- "^(.*?)(\\d+\\w*)?$$"

  # Use strcapture to extract the parts
  parts <- utils::strcapture(pattern, address, proto = list(STRNAME = character(), DEINR = character()))

  # Trim leading and trailing spaces from the street name
  parts$STRNAME <- trimws(parts$STRNAME)

  return(parts)
}

#' Split Addresses into Street Name and House Number in a Data Frame
#'
#' This function takes a data frame with a column containing addresses and splits each address into two components: the street name and the house number. The street is everything before the last sequence of digits which may include letters (e.g., "54A" or "10A").
#'
#' @param data A data frame containing a column with addresses.
#' @param col_name The name of the column containing the addresses.
#' @return A data frame with the original addresses and two new columns: $street containing the street name, and $number containing the house number.
split_addresses <- function(data, col_name = "Addresses") {
  data <- cbind(data, do.call(rbind, lapply(data[[col_name]], split_address)))
  return(data)
}

# --------------------- #
# ------- MAIN -------- #
# --------------------- #

file_in <- file.choose()
IAZY_df <- read_excel(file_in)

IAZY_df <- split_addresses(IAZY_df, "IAZI_STREET")

file_out <- file.choose()
write_xlsx(IAZY_df, file_out)
