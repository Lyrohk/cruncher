#' Function to lookup a card for a single organization over the Entity Lookup API Endpoint
#'
#'@param id UUID or permalink of the organization you wish to look up
#'@param card card field of interest that will be returned. Only one please!
#'@param parse TRUE or FALSE. By default TRUE. If set to FALSE, it will return the data directly from the JSON, if set to TRUE, it will parse it into a data.frame object
#'@return either a data.frame (if parse = TRUE) or a list (if parse = FALSE)
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' lookUpDetailForOrganization(id = "facebook", card = "investors")
#'
#' @import httr
#' @import jsonlite
#' @import stringr
#' @import dplyr
#' @export
#'
# DetailedLookOrganizationForCard
lookupDetailForOrganization <- function(id, card, please_parse = TRUE) {

  # Check that card is in list of cards
  if (!card %in% c("acquiree_acquisitions",
                  "acquirer_acquisitions",
                  "child_organizations",
                  "child_ownerships",
                  "event_appearances",
                  "founders",
                  "headquarters_address",
                  "investors",
                  "ipos",
                  "jobs",
                  "parent_organization",
                  "parent_ownership",
                  "participated_funding_rounds",
                  "participated_funds",
                  "participated_investments",
                  "press_references",
                  "raised_funding_rounds",
                  "raised_funds",
                  "raised_investments")) {
    print("The card needs to be an organizational card. Pick one of the following:")
    getOrganizationCards()
    stop()
  }

  # Check that API_KEY exists
  if (!exists("API_KEY")) {
    stop("Please set a valid user key to API_KEY as an environmental variable or for use setAPIKey() to do it for you.")
  }

  # Check that id has been specified
  if (missing(id)) {
    stop("Please enter a uuid or permalink for the person you wish to look up. You can find them manually from the browser or csv files.")
  }

  # Create the path
  url <- paste0("https://api.crunchbase.com/api/v4/entities/organizations/", id, "/cards/", card, "?user_key=", API_KEY)

  # Make http GET request
  response <- RETRY(verb = "GET", url = url) # Could use GET but someone may apply it with a list

  # Check if we get valid data, if not return error core
  if (response$status_code == 200) {
    data <- fromJSON(rawToChar(response$content))
    # Check if parsing is wanted
    if (please_parse) {

      # Initialize df

      # Based on card, parse differently
      if (card %in% c("acquiree_acquisitions", "acquirer_acquisitions")) {
        parseAquisition(data)
      } else if (card %in% c("child_organizations","parent_organization") ) {
        parseOrganization(data)
      } else if (card %in% c("founders","") ) {
        parsePerson(data)
      } else if (card %in% c("participated_funding_rounds", "raised_funding_rounds") ) {
        parseFundingRound(data)
      } else if (card %in% c("child_ownerships", "parent_ownership") ) {
        return(parseOwnership(data))
      } else if (card %in% c("ipos") ) {
        parseIpo(data)
      } else if (card %in% c("participated_investments", "raised_investments") ) {
        parseInvestment(data)
      } else if (card %in% c("participated_funds", "raised_funds") ) {
        parseFund(data)
      } else if (card %in% c("press_references") ) {
        parsePressReference(data)
      } else if (card %in% c("event_appearances") ) {
        parseEventAppearance(data)
      } else if (card %in% c("jobs") ) {
        parseJob(data)
      } else if (card %in% c("headquarters_address") ) {
        parseAddress(data)
      } else {
        # TODO Parse PRINIPAL TYPE OF "investors"
      }

      # Check length of df (if 100, get next df and rbind to original)




    } else {
      # Return data
      return(data)
    }
  } else {
    # Print error code
    printError(response$status_code)
  }
}