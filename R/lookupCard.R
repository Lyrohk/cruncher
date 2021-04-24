#' Function to lookup a card for a single organization over the Entity Lookup API Endpoint
#'
#' Takes the entity id to lookup a card and paginate through the entities and parse them into a data.frame
#'
#'@param id UUID or permalink of the organization you wish to look up
#'@param path Type of entity e.g. organizations
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
#'
lookupCard <- function(id, path, card, initial_data) {

  # Quick to failure section ####

  # Check that all arguments exists
  if (missing(id)) {
    stop("An entity uuid or permalink needs to be specified in the (hidden) lookupCard function.")
  }
  if (missing(path)) {
    stop("A path needs to be specified in the (hidden) lookupCard function.")
  }
  if (missing(card)) {
    stop("A card needs to be specified in the (hidden) lookupCard function.")
  }

  # Check that path is part of the available ones
  if (!path %in% getPaths()) {
    stop("Path needs to be one of the available ones. Call getPaths() to check 'em out.")
  }

  # Check that card is within the path's card
  if (!card %in% getCardsForPath(path)) {
    stop("Card needs to one of the path's card. Call getCardsForPath('yourPath') to check the available ones.")
  }

  # Entity id check
  id <- entityIdCheck(id)

  # Get right parser
  parsingFun <- getParserForCard(card)

  # Let the lookup begin! ####

  # Get first 100 results

  # If 100, get next until all are returned

  # Return final data.frame or list




}