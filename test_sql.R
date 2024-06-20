# Install the RSQLite package if it's not already installed
if (!requireNamespace("RSQLite", quietly = TRUE)) {
  install.packages("RSQLite")
}

# Load the RSQLite package
library(RSQLite)

devtools::load_all(".")

# Load the RSQLite package
library(RSQLite)

# Specify the path to the SQLite database
db_path <- "C:/Users/Maxime/Swiss Climate AG/Projet Hypothèques - General/22_RegBl/databse/ch-202406201455/data.sqlite"

# Establish a connection to the SQLite database
conn <- dbConnect(RSQLite::SQLite(), dbname = db_path)

# Check if the connection was successful
if (!dbIsValid(conn)) {
  stop("Failed to connect to the database.")
}

# Print a message confirming the connection
cat("Connected to the SQLite database successfully.\n")

# Optionally, list all tables in the database
# tables <- dbListTables(conn)
# cat("Tables in the database:\n")
# print(tables)

# # Get all columns in the "building" table
# columns <- dbListFields(conn, "building")

# # Print the columns
# cat("Columns in the 'building' table:\n")
# print(columns)
# Query the database to get the desired rows
query <- "SELECT * FROM building WHERE GGDENAME = 'Biel/Bienne' AND LPARZ = 2207"
result <- dbGetQuery(conn, query)

# Print the result
print(result)


# Remember to close the connection when done
dbDisconnect(conn)
cat("Connection to the SQLite database closed.\n")