#' Request Building Information from RegBl
#'
#' This function sends a request to the RegBl API to retrieve detailed information about a building.
#' The request can be made using either the building's unique identifier (EGID) or its address components
#' (DEINR, STRNAME, DPLZ4). The function constructs an XML request, sends it to the RegBl API, and processes
#' the response to extract and return the building information.
#'
#' @param building A list or data frame representing the building, containing at least EGID or address components (DEINR, STRNAME, and DPLZ4).
#'
#' @return A modified version of the input building object, with additional information filled in from the RegBl response.
#'         If no unique building is found, or if an error occurs during the request, the function stops with an error message.
#'
#' @examples
#' building <- list(EGID = 123456)
#' updated_building <- request_regbl_web(building)
#'
#' @details
#' The function first determines the type of request based on available information in the `building` object:
#' - If EGID is available, it constructs a request based on the building's EGID.
#' - If EGID is not available, but address components are, it constructs a request based on the building's address.
#'
#' The function then assembles the XML request body, including a unique message ID, application details, and
#' the appropriate query parameters. After sending the request to the RegBl API endpoint and receiving a response,
#' the function checks for errors and, if successful, extracts relevant data from the response XML to fill in
#' the building information. It also logs the XML response for record-keeping and further validation.
#'
#' The function requires that the `.constants$buildings_df_columns` object is defined elsewhere in the package
#' and contains necessary metadata about the expected building data fields, including their corresponding xpaths
#' in the RegBl response XML schema.
#'
#' For more information about the RegBl MADD API, refer to:
#' \url{https://www.housing-stat.ch/fr/madd/restricted/ech-0206.html}
#'
#' For details on finding XPath and names of a node in the web service request from RegBl, consult the eCH-0206 standard:
#' \url{https://www.housing-stat.ch/files/STAN_f_DEF_2022-06-18_eCH-0206_V2.0.0_Donnees_RegBL_aux_tiers.pdf}
#'
#' Variable names for RegBl data are taken from the "Catalogue des caractères - Registre fédéral des bâtiments et des logements 4.2":
#' \url{https://www.bfs.admin.ch/asset/fr/22905271}
#' @importFrom magrittr %>%
request_regbl_web <- function(building) {
  # Check if the building data is complete
  if (!is.na(building$EGID)) {
    request_type <- "egid"
  } else if (!is.na(building$DEINR) && !is.na(building$STRNAME) && !is.na(building$DPLZ4)) {
    request_type <- "address"
  } else if (!is.na(building$LPARZ) && !is.na(building$STRNAME) && !is.na(building$DPLZ4)) {
    request_type <- "parcel"
  } else {
    stop("Building data is incomplete. Need EGID, or DEINR/STRNAME/DPLZ4, or LPARZ/STRNAME/DPLZ4 to found the building in RegBl.")
  }

  # API request global parameters
  madd_url <- "https://madd.bfs.admin.ch/eCH-0206"
  pkg_name <- "BuildingsEmissionsCalculator"
  pkg_version <- packageVersion("BuildingsEmissionsCalculator")
  business_id <- "9876543210" # TODO choose a unique business id
  manufacturer <- "SwissClimateAG"
  request_datetime <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S", tz = "UTC")

  # Create the XML request body
  if (request_type == "egid") {
    message_id <- sanitize_filename(gsub(" ", "_", paste(building$EGID, ids::uuid() %>% substr(1, 4), sep = "_"))) # generate a unique readable message id. Char such as \/:*?"<>| are removed.
    # XML request body
    request_body <- paste0('<?xml version="1.0" encoding="UTF-8"?>
<eCH-0206:maddRequest xmlns:eCH-0058="http://www.ech.ch/xmlns/eCH-0058/5" xmlns:eCH-0206="http://www.ech.ch/xmlns/eCH-0206/2" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.ech.ch/xmlns/eCH-0206/2 eCH-0206-2-0.xsd">
     <eCH-0206:requestHeader>
   	   	<eCH-0206:messageId>', message_id, "</eCH-0206:messageId>
   	   	<eCH-0206:businessReferenceId>", business_id, "</eCH-0206:businessReferenceId>
   	   	<eCH-0206:requestingApplication>
   	   	   	<eCH-0058:manufacturer>", manufacturer, "</eCH-0058:manufacturer>
   	   	   	<eCH-0058:product>", pkg_name, "</eCH-0058:product>
   	   	   	<eCH-0058:productVersion>", pkg_version, "</eCH-0058:productVersion>
   	   	</eCH-0206:requestingApplication>
   	   	<eCH-0206:requestDate>", request_datetime, "</eCH-0206:requestDate>
   	</eCH-0206:requestHeader>
   	<eCH-0206:requestContext>building</eCH-0206:requestContext>
   	<eCH-0206:requestQuery>
   	   	<eCH-0206:EGID>", building$EGID, "</eCH-0206:EGID>
   	</eCH-0206:requestQuery>
</eCH-0206:maddRequest>")
  } else if (request_type == "address") {
    message_id <- sanitize_filename(gsub(" ", "_", paste(building$DPLZ4, building$STRNAME %>% substr(1, 36 - 15), building$DEINR, ids::uuid() %>% substr(1, 4), sep = "_"))) # generate a unique readable message id. The max length is 36 characters. Char such as \/:*?"<>| are removed.
    # XML request body
    request_body <- paste0('<?xml version="1.0" encoding="UTF-8"?>
	<eCH-0206:maddRequest xmlns:eCH-0058="http://www.ech.ch/xmlns/eCH-0058/5" xmlns:eCH-0206="http://www.ech.ch/xmlns/eCH-0206/2" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.ech.ch/xmlns/eCH-0206/2 eCH-0206-2-0.xsd">
		<eCH-0206:requestHeader>
			<eCH-0206:messageId>', message_id, "</eCH-0206:messageId>
			<eCH-0206:businessReferenceId>", business_id, "</eCH-0206:businessReferenceId>
			<eCH-0206:requestingApplication>
				<eCH-0058:manufacturer>", manufacturer, "</eCH-0058:manufacturer>
				<eCH-0058:product>", pkg_name, "</eCH-0058:product>
				<eCH-0058:productVersion>", pkg_version, "</eCH-0058:productVersion>
			</eCH-0206:requestingApplication>
			<eCH-0206:requestDate>", request_datetime, "</eCH-0206:requestDate>
		</eCH-0206:requestHeader>
		<eCH-0206:requestContext>building</eCH-0206:requestContext>
		<eCH-0206:requestQuery>
			<eCH-0206:condition>
					<eCH-0206:attributePath>/eCH-0206:maddResponse/eCH-0206:buildingList/eCH-0206:buildingItem/eCH-0206:buildingEntranceList/eCH-0206:buildingEntranceItem/eCH-0206:buildingEntrance/eCH-0206:buildingEntranceNo</eCH-0206:attributePath>
					<eCH-0206:operator>equalTo</eCH-0206:operator>
					<eCH-0206:attributeValue>", building$DEINR, "</eCH-0206:attributeValue>
				</eCH-0206:condition>
			<eCH-0206:condition>
					<eCH-0206:attributePath>/eCH-0206:maddResponse/eCH-0206:buildingList/eCH-0206:buildingItem/eCH-0206:buildingEntranceList/eCH-0206:buildingEntranceItem/eCH-0206:buildingEntrance/eCH-0206:street/eCH-0206:streetNameList/eCH-0206:streetNameItem/eCH-0206:descriptionLong</eCH-0206:attributePath>
					<eCH-0206:operator>equalTo</eCH-0206:operator>
					<eCH-0206:attributeValue>", building$STRNAME, "</eCH-0206:attributeValue>
				</eCH-0206:condition>
			<eCH-0206:condition>
					<eCH-0206:attributePath>/eCH-0206:maddResponse/eCH-0206:buildingList/eCH-0206:buildingItem/eCH-0206:buildingEntranceList/eCH-0206:buildingEntranceItem/eCH-0206:buildingEntrance/eCH-0206:locality/eCH-0206:swissZipCode</eCH-0206:attributePath>
					<eCH-0206:operator>equalTo</eCH-0206:operator>
					<eCH-0206:attributeValue>", building$DPLZ4, "</eCH-0206:attributeValue>
				</eCH-0206:condition>
		</eCH-0206:requestQuery>
	</eCH-0206:maddRequest>")
  } else if (request_type == "parcel") {
    message_id <- sanitize_filename(gsub(" ", "_", paste(building$DPLZ4, building$LPARZ, ids::uuid() %>% substr(1, 4), sep = "_"))) # generate a unique readable message id. The max length is 36 characters. Char such as \/:*?"<>| are removed.
    # XML request body
    request_body <- paste0(
                           '<?xml version="1.0" encoding="UTF-8"?>
                             <eCH-0206:maddRequest xmlns:eCH-0058="http://www.ech.ch/xmlns/eCH-0058/5" xmlns:eCH-0206="http://www.ech.ch/xmlns/eCH-0206/2" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.ech.ch/xmlns/eCH-0206/2 eCH-0206-2-0.xsd">
                               <eCH-0206:requestHeader>
                                 <eCH-0206:messageId>', message_id, "</eCH-0206:messageId>
                                 <eCH-0206:businessReferenceId>", business_id, "</eCH-0206:businessReferenceId>
                                 <eCH-0206:requestingApplication>
                                   <eCH-0058:manufacturer>", manufacturer, "</eCH-0058:manufacturer>
                                   <eCH-0058:product>", pkg_name, "</eCH-0058:product>
                                   <eCH-0058:productVersion>", pkg_version, "</eCH-0058:productVersion>
                                 </eCH-0206:requestingApplication>
                                 <eCH-0206:requestDate>", request_datetime, "</eCH-0206:requestDate>
                               </eCH-0206:requestHeader>
                               <eCH-0206:requestContext>building</eCH-0206:requestContext>
                               <eCH-0206:requestQuery>
                                 <eCH-0206:condition>
                                     <eCH-0206:attributePath>/eCH-0206:maddResponse/eCH-0206:buildingList/eCH-0206:buildingItem/eCH0206:realestateIdentificationList/eCH-0206:realestateIdentificationItem/eCH0206:number</eCH-0206:attributePath>
                                     <eCH-0206:operator>equalTo</eCH-0206:operator>
                                     <eCH-0206:attributeValue>", building$LPARZ, "</eCH-0206:attributeValue>
                                   </eCH-0206:condition>
                                 <eCH-0206:condition>
                                     <eCH-0206:attributePath>/eCH-0206:maddResponse/eCH-0206:buildingList/eCH-0206:buildingItem/eCH-0206:buildingEntranceList/eCH-0206:buildingEntranceItem/eCH-0206:buildingEntrance/eCH-0206:street/eCH-0206:streetNameList/eCH-0206:streetNameItem/eCH-0206:descriptionLong</eCH-0206:attributePath>
                                     <eCH-0206:operator>equalTo</eCH-0206:operator>
                                     <eCH-0206:attributeValue>", building$STRNAME, "</eCH-0206:attributeValue>
                                   </eCH-0206:condition>
                                 <eCH-0206:condition>
                                     <eCH-0206:attributePath>/eCH-0206:maddResponse/eCH-0206:buildingList/eCH-0206:buildingItem/eCH-0206:buildingEntranceList/eCH-0206:buildingEntranceItem/eCH-0206:buildingEntrance/eCH-0206:locality/eCH-0206:swissZipCode</eCH-0206:attributePath>
                                     <eCH-0206:operator>equalTo</eCH-0206:operator>
                                     <eCH-0206:attributeValue>", building$DPLZ4, "</eCH-0206:attributeValue>
                                   </eCH-0206:condition>
                               </eCH-0206:requestQuery>
                             </eCH-0206:maddRequest>")
  }    

  # Send a POST request to the API
  response <- httr2::request(madd_url) %>%
    httr2::req_headers("Content-Type" = "text/xml") %>%
    httr2::req_body_raw(request_body) %>%
    httr2::req_perform()

  # Check if the request was successful
  if (response$status_code != 200) {
    stop(paste("HTTP request failed with status code", response$status_code))
  }

  # Parse the XML response
  xml_content <- response %>% httr2::resp_body_xml()
  print(xml_content)
  # Save reply to a log file
  if (.constants$saveLogs) {
    xml2::write_xml(xml_content, paste0("log/", message_id, ".xml"))
    
  }

  # Check XML response status. See https://www.housing-stat.ch/files/error_codes_flags.xlsx
  xml_status_code <- xml_content %>%
    xml2::xml_find_first(".//d1:code") %>%
    xml2::xml_text()
  xml_status_message <- xml_content %>%
    xml2::xml_find_first(".//d1:message") %>%
    xml2::xml_text()
  # Code between 100..199 is for positive reply
  # Code between 200..399 is for server errors
  # Code between 400..700 is for client errors
  if (xml_status_code >= 100 && xml_status_code < 200) {
    # message(paste("Request was successful:", xml_status_code, xml_status_message))
  } else if (xml_status_code >= 200 && xml_status_code < 400) {
    stop(paste("Server error:", xml_status_code, xml_status_message))
  } else if (xml_status_code >= 400 && xml_status_code < 701) {
    stop(paste("Client error:", xml_status_code, xml_status_message))
  } else {
    stop(paste("Unknown error:", xml_status_code, xml_status_message))
  }

  # Check if a unique building was found
  building_found <- xml_content %>%
    xml2::xml_find_first(".//d1:objectCount") %>%
    xml2::xml_integer()
  if (building_found == 0) {
    stop("No building found")
  } else if (building_found > 1) {
    stop(paste("More than one building found :", building_found))
  }

  ## Extract data from the XML response
  # Required data

  for (col_name in names(building)) {
    # Skip if the column is already filled
    if (is.na(building[[col_name]])) {
      column_info <- .constants$buildings_df_columns[[col_name]]

      # Skip if column_info or xpath is unknown
      if (!is.null(column_info$xpath)) {
        # Depending on the type, use the appropriate xml2 extraction function
        if (column_info$type == "integer") {
          building[[col_name]] <- xml_content %>%
            xml2::xml_find_first(column_info$xpath) %>%
            xml2::xml_integer()
        } else if (column_info$type == "character") {
          building[[col_name]] <- xml_content %>%
            xml2::xml_find_first(column_info$xpath) %>%
            xml2::xml_text()
        } else if (column_info$type == "date") {
          building[[col_name]] <- xml_content %>%
            xml2::xml_find_first(column_info$xpath) %>%
            xml2::xml_text() # TODO Additional conversion may be required
        } else if (column_info$type == "double") {
          building[[col_name]] <- xml_content %>%
            xml2::xml_find_first(column_info$xpath) %>%
            xml2::xml_double()
        }
      }
    }
  }

  return(building)
}
