# For more info about the RegBl MADD API see: https://www.housing-stat.ch/fr/madd/restricted/ech-0206.html
# To found XPath and names of a node in the web service request from RegBl, the eCH-0206 standard is used. See: https://www.housing-stat.ch/files/STAN_d_DRA_2020-07-21_eCH-0206_V2.0_GWR-Daten_an_Dritte_Draft-0.99.pdf
# Variable names for RegBl data come from the "Catalogue des caractères - Registre fédéral des bâtiments et des logements 4.2" https://www.bfs.admin.ch/asset/fr/22905271
# TODO 2nd heating and 2nd hot water system not taken into account


# Load necessary libraries
library(httr2)
library(xml2)
library(ids)

building <- list()
building$DEINR <- 8
building$STRNAME <- "Rue des Diamants"
building$DPLZ4 <- 2503

# create a request function
request_regbl <- function(building) {
	# Check if the building data is complete
	if (is.null(building$DEINR) || is.null(building$STRNAME) || is.null(building$DPLZ4)) {
		stop("Building data is incomplete")
	}

    # API request parameters
    madd_url <- "https://madd.bfs.admin.ch/eCH-0206"
    pkg_name <- "FinancedEmissionsCalculator" # TODO get the package name from the DESCRIPTION file
    pkg_version <- "0.0.0.9000" # TODO get the version from the DESCRIPTION file
    message_id <- gsub(" ", "_", paste(building$DPLZ4, building$STRNAME, building$DEINR, uuid() %>% substr(1, 6), sep = "_")) # generate a unique readable message id
    business_id <- "9876543210" # TODO choose a unique business id
    manufacturer <- "SwissClimateAG"
    request_datetime <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S", tz = "UTC")

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

    # Send a POST request to the API
    response <- request(madd_url) %>%
        req_headers("Content-Type" = "text/xml") %>%
        req_body_raw(request_body) %>%
        req_perform()

    # Check if the request was successful
    if (response$status_code != 200) {
        stop(paste("HTTP request failed with status code", response$status_code))
    }

    # Parse the XML response
    xml_content <- response %>% resp_body_xml()
    xml_namespaces <- xml_content %>% xml_ns()
    # Get response datetime
    response_datetime <- response %>% resp_header("Date")

    # Check XML response status. See https://www.housing-stat.ch/files/error_codes_flags.xlsx
    xml_status_code <- xml_content %>%
        xml_find_first(".//d1:code") %>%
        xml_text()
    xml_status_message <- xml_content %>%
        xml_find_first(".//d1:message") %>%
        xml_text()
    # Code between 100..199 is for positive reply
    # Code between 200..399 is for server errors
    # Code between 400..700 is for client errors
    if (xml_status_code >= 100 && xml_status_code < 200) {
        message(paste("Request was successful:", xml_status_code, xml_status_message))
    } else if (xml_status_code >= 200 && xml_status_code < 400) {
        stop(paste("Server error:", xml_status_code, xml_status_message))
    } else if (xml_status_code >= 400 && xml_status_code < 700) {
        stop(paste("Client error:", xml_status_code, xml_status_message))
    } else {
        stop(paste("Unknown error:", xml_status_code, xml_status_message))
    }

    # Check if a building was found
    building_found <- xml_content %>%
        xml_find_first(".//d1:objectCount") %>%
        xml_integer()
    if (building_found == 0) {
        stop("No building found")
    } else if (building_found > 1) {
        stop(paste("More than one building found :", building_found))
    }

    # Extract data from the XML response
    building$EGID <- xml_content %>%
        xml_find_first(".//d1:EGID") %>%
        xml_text()
    building$GKAT <- xml_content %>%
        xml_find_first(".//d1:buildingCategory") %>%
        xml_text()
    building$GBAUJ <- xml_content %>%
        xml_find_first(".//d1:dateOfConstruction/d1:dateOfConstruction") %>%
        xml_text()
    building$GBAUP <- xml_content %>%
        xml_find_first(".//d1:dateOfConstruction/d1:periodOfConstruction") %>%
        xml_text()
    building$GABBJ <- xml_content %>%
        xml_find_first(".//d1:yearOfDemolition") %>%
        xml_text()
    building$GAREA <- xml_content %>%
        xml_find_first(".//d1:surfaceAreaOfBuilding") %>%
        xml_text()
    building$GASTW <- xml_content %>%
        xml_find_first(".//d1:numberOfFloors") %>%
        xml_text()
    building$GEBF <- xml_content %>%
        xml_find_first(".//d1:energyRelevantSurface") %>%
        xml_text()

    building$GWAERZH1 <- xml_content %>%
        xml_find_first(".//d1:thermotechnicalDeviceForHeating1/d1:heatGeneratorHeating") %>%
        xml_text()
    building$GENH1 <- xml_content %>%
        xml_find_first(".//d1:thermotechnicalDeviceForHeating1/d1:energySourceHeating") %>%
        xml_text()
    building$GWAERSCEH1 <- xml_content %>%
        xml_find_first(".//d1:thermotechnicalDeviceForHeating1/d1:informationSourceHeating") %>%
        xml_text()
    building$GWAERDATH1 <- xml_content %>%
        xml_find_first(".//d1:thermotechnicalDeviceForHeating1/d1:revisionDate") %>%
        xml_text()

    building$GWAERZH2 <- xml_content %>%
        xml_find_first(".//d1:thermotechnicalDeviceForHeating2/d1:heatGeneratorHeating") %>%
        xml_text()
    building$GENH2 <- xml_content %>%
        xml_find_first(".//d1:thermotechnicalDeviceForHeating2/d1:energySourceHeating") %>%
        xml_text()
    building$GWAERSCEH2 <- xml_content %>%
        xml_find_first(".//d1:thermotechnicalDeviceForHeating2/d1:informationSourceHeating") %>%
        xml_text()
    building$GWAERDATH2 <- xml_content %>%
        xml_find_first(".//d1:thermotechnicalDeviceForHeating2/d1:revisionDate") %>%
        xml_text()

    building$GWAERZW1 <- xml_content %>%
        xml_find_first(".//d1:thermotechnicalDeviceForWarmWater1/d1:heatGeneratorHotWater") %>%
        xml_text()
    building$GENW1 <- xml_content %>%
        xml_find_first(".//d1:thermotechnicalDeviceForWarmWater1/d1:energySourceHeating") %>%
        xml_text()
    building$GWAERSCEW1 <- xml_content %>%
        xml_find_first(".//d1:thermotechnicalDeviceForWarmWater1/d1:informationSourceHeating") %>%
        xml_text()
    building$GWAERDATW1 <- xml_content %>%
        xml_find_first(".//d1:thermotechnicalDeviceForWarmWater1/d1:revisionDate") %>%
        xml_text()

    building$GWAERZW2 <- xml_content %>%
        xml_find_first(".//d1:thermotechnicalDeviceForWarmWater2/d1:heatGeneratorHotWater") %>%
        xml_text()
    building$GENW2 <- xml_content %>%
        xml_find_first(".//d1:thermotechnicalDeviceForWarmWater2/d1:energySourceHeating") %>%
        xml_text()
    building$GWAERSCEW2 <- xml_content %>%
        xml_find_first(".//d1:thermotechnicalDeviceForWarmWater2/d1:informationSourceHeating") %>%
        xml_text()
    building$GWAERDATW2 <- xml_content %>%
        xml_find_first(".//d1:thermotechnicalDeviceForWarmWater2/d1:revisionDate") %>%
        xml_text()

    # Save reply to a log file
    write_xml(xml_content, paste0("log/", message_id, ".xml"))

    return(building)
}
