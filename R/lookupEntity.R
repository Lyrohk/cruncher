#' Function to lookup a single entity over the Entity Lookup API Endpoint
#'
#'@param id UUID or permalink of the person you wish to look up
#'@param path The path to the entity e.g. organizations, people, etc.
#'@param please_parse TRUE or FALSE. By default TRUE. If set to FALSE, it will return the data directly from the JSON, if set to TRUE, it will parse it into a data.frame object
#'@return either a data.frame (if parse = TRUE) or a list (if parse = FALSE)
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' lookupEntity("facebook", "organizations", please_parse = T)
#'
#' @import httr
#' @import jsonlite
#'
#' @export
#'
lookupEntity <- function(id, path, please_parse = TRUE) {
  # Check that API_KEY exists
  if (!exists("API_KEY")) {
    stop("Please set a valid user key to API_KEY as an environmental variable or for use setAPIKey() to do it for you.")
  }

  # Check that id has been specified
  if (missing(id)) {
    stop("Please enter a uuid or permalink for the person you wish to look up. You can find them manually from the browser or csv files.")
  }

  # Entity id check e.g. to lower, trimming whitespace, and replacing spaces with -
  id <- entityIdCheck(id)

  # Check that path is in the possible range

  # Create the path
  url <- paste0("https://api.crunchbase.com/api/v4/entities/", path, "/", id, "?card_ids=fields&user_key=", API_KEY)

  # Make http GET request
  response <- RETRY(verb = "GET", url = url) # Could use GET but someone may apply it with a list

  # Check if we get valid data, if not return error core
  if (response$status_code == 200) {
    data <- fromJSON(rawToChar(response$content))
    # Check if parsing is wanted
    if (please_parse) {

      # Extract out the property fields
      field_data <- data$cards$fields

      # TODO COmplete and hook up to individual lookupEntity functions to reduce boilerplate code
      # Parse depending on path to return a dataframe
      if (path == "organizations") {
        return(parseOrganization(field_data))
      } else if (path == "people") {
        return(parsePerson(field_data))
      } else if (path == "acquisitions") {
        return(parseAquisition(field_data))
      } else if (path == "investments") {
        return(parseInvestment(field_data))
      } else if (path == "events") {
        return(parseEvent(field_data))
      } else if (path == "event_appearances") {
        return(parseEventAppearance(field_data))
      } else if (path == "funds") {
        return(parseFund(field_data))
      } else if (path == "funding_rounds") {
        return(parseFundingRound(field_data))
      } else if (path == "ipos") {
        return(parseIpo(field_data))
      } else if (path == "jobs") {
        return(parseJob(field_data))
      } else if (path == "degrees") {
        return(parseDegree(field_data))
      } else if (path == "press_references") {
        return(parsePressReference(field_data))
      } else if (path == "categories") {
        return(parseCategory(field_data))
      } else if (path == "category_groups") {
        return(parseCategoryGroup(field_data))
      } else if (path == "ownerships") {
        return(parseOwnership(field_data))
      } else if (path == "locations") {
        return(parseLocation(field_data))
      } else {
        # Path was not recognized, stop here
        stop("Path was not recognized. Please check your spelling before trying again.")
      }
    } else {
      # Return data
      return(data)
    }
  } else {
    # Print error code
    printError(response$status_code)
  }
}