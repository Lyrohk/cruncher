#' Function to lookup and paginate through a single card for a single event appearance over the Entity Lookup API Endpoint
#'
#' Takes the entity id to lookup a card and paginate through the entities and parse them into a data.frame
#'
#'@param event_appearance_card card field of interest that will be returned. Only one please!
#'@param event_appearance_id UUID or permalink of the event appearance you wish to look up
#'@return a data.frame
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' lookupEventAppearanceCard(event_appearance_card = "card", event_appearance_id = "id")
#'
#'@export
#'
lookupEventAppearanceCard <- function(event_appearance_card, event_appearance_id) {
  if (length(event_appearance_id) == 0) {
    stop("Please provide a valid event_appearance_id.")
  } else if (length(event_appearance_id) == 1) {
    # Lookup the cards for a single id
    return(lookupEntityCard(entity_card = event_appearance_card, entity_id = event_appearance_id, entity_path = "event_appearances"))
  } else {
    # There are multiple ids
    return(lapply(event_appearance_id, lookupEntityCard, entity_card = event_appearance_card, entity_path = "event_appearances"))
  }
}