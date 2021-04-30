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
  if (length(event_id) == 0) {
    stop("Please provide a valid event_id.")
  } else if (length(event_id) == 1) {
    # Lookup the cards for a single id
    return(lookupEntityCard(entity_card = event_card, entity_id = event_id, entity_path = "events"))
  } else {
    # Add duplicate and time check
    duplicateTimeCheck(event_id)

    # There are multiple ids
    return(silenceFun(lapply(event_id, lookupEntityCard, entity_card = event_card, entity_path = "events")))
  }
}