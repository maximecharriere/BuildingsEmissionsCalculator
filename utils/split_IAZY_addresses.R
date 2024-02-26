file_in <- file.choose()
IAZY_df <- read_excel(file_in)

IAZY_df <- split_addresses(IAZY_df, "IAZI_STREET")

file_out <- file.choose()
write_xlsx(IAZY_df, file_out)
