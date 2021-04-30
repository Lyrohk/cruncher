#' Function to lookup and paginate through a single card for a single event over the Entity Lookup API Endpoint
#'
#' Takes the entity id to lookup a card and paginate through the entities and parse them into a data.frame
#'
#'@param event_card card field of interest that will be returned. Only one please!
#'@param event_id UUID or permalink of the event you wish to look up
#'@param please_parse Logical. By default TRUE and will parse your data from a list of data.frames to a final data.frame with empty elements dropping out.
#'@return a data.frame
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#'
#'@export
#'
lookupEventCard <- function(event_card, event_id, please_parse = TRUE) {
  if (length(event_id) == 0) {
    stop("Please provide a valid event_id.")
  } else if (length(event_id) == 1) {
    # Lookup the cards for a single id
    return(lookupEntityCard(entity_card = event_card, entity_id = event_id, entity_path = "events"))
  } else {
    # Add duplicate and time check
    duplicateTimeCheck(event_id)

    # Check please_parse
    if (please_parse) {
      # Bind data into a final data.frame with those elements without data dropping out
      return( rbind_pages(silenceFun(lapply(event_id, lookupEntityCard, entity_card = event_card, entity_path = "events"))) )
    } else {
      # Return a list of (potentially empty) data.frames
      return(silenceFun(lapply(event_id, lookupEntityCard, entity_card = event_card, entity_path = "events")))
    }
  }
}