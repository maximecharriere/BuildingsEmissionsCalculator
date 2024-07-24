#' Download and Prepare SQLite Database
#'
#' This function downloads a ZIP file containing a SQLite database and related files from a specified URL.
#' It extracts the database file, moves it to a specified file path, and creates an index on the 'entrance' table.
#'
#' @param db_filepath A character string specifying the file path where the SQLite database should be saved.
#' @export
#' @examples
#' \dontrun{
#' # Example usage:
#' update_regbl_db("path/to/your/database.sqlite")
#' }
update_regbl_db <- function(db_filepath) {
  # Define the temporary directory and file paths
  temp_dir <- tempfile()
  temp_zip <- file.path(temp_dir, "ch.zip")
  
  # Create the temporary directory
  dir.create(temp_dir)
  
  # Step 1: Download the data
  print("Downloading the RegBl database...")
  download.file("https://public.madd.bfs.admin.ch/ch.zip", temp_zip, mode = "wb", quiet = FALSE)
  
  # Step 2: Extract the data
  print("Extracting the RegBl database...")
  utils::unzip(temp_zip, exdir = temp_dir)
  
  # Step 3: Move data.sqlite to the desired location
  extracted_db_path <- file.path(temp_dir, "data.sqlite")
  if (file.exists(extracted_db_path)) {
    file.copy(extracted_db_path, db_filepath, overwrite = TRUE)
  } else {
    stop("data.sqlite file not found in the extracted contents.")
  }
  
  # Step 4: Remove the temporary directory and zip file
  unlink(temp_dir, recursive = TRUE)
  
  # Step 5: Create the index on the SQLite database
  print("Creating index in the RegBl database...")
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = db_filepath)
  RSQLite::dbExecute(conn, 'CREATE INDEX IF NOT EXISTS "IX_ENTRANCE_STREET" ON "entrance" ("DPLZ4","STRNAME","DEINR");')
  RSQLite::dbDisconnect(conn)

  print("RegBl database update complete.")
}