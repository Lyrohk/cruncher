#' Function to lookup and paginate through a single card for a single ipo over the Entity Lookup API Endpoint
#'
#' Takes the entity id to lookup a card and paginate through the entities and parse them into a data.frame
#'
#'@param ipo_card card field of interest that will be returned. Only one please!
#'@param ipo_id UUID or permalink of the ipo you wish to look up
#'@return a data.frame
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' lookupIpoCard(ipo_card = "card", ipo_id = "id")
#'
#'@export
#'
lookupIpoCard <- function(ipo_card, ipo_id) {
  if (length(ipo_id) == 0) {
    stop("Please provide a valid ipo_id.")
  } else if (length(ipo_id) == 1) {
    # Lookup the cards for a single id
    return(lookupEntityCard(entity_card = ipo_card, entity_id = ipo_id, entity_path = "ipos"))
  } else {
    # There are multiple ids
    return(lapply(ipo_id, lookupEntityCard, entity_card = ipo_card, entity_path = "ipos"))
  }
}