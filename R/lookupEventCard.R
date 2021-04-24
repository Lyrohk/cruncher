#' Function to lookup and paginate through a single card for a single event over the Entity Lookup API Endpoint
#'
#' Takes the entity id to lookup a card and paginate through the entities and parse them into a data.frame
#'
#'@param event_card card field of interest that will be returned. Only one please!
#'@param event_id UUID or permalink of the event you wish to look up
#'@return a data.frame
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#'
#'@export
#'
lookupEventCard <- function(event_card, event_id) {
  return(lookupEntityCard(entity_card = event_card, entity_id = event_id, entity_path = "events"))
}