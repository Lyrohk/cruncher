#' Function to lookup and paginate through a single card for a single ownership over the Entity Lookup API Endpoint
#'
#' Takes the entity id to lookup a card and paginate through the entities and parse them into a data.frame
#'
#'@param ownership_card card field of interest that will be returned. Only one please!
#'@param ownership_id UUID or permalink of the ownership you wish to look up
#'@param please_parse Logical. By default TRUE and will parse your data from a list of data.frames to a final data.frame with empty elements dropping out.
#'@return a data.frame
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' lookupOwnershipCard(ownership_card = "card", ownership_id = "id")
#'
#'@export
#'
lookupOwnershipCard <- function(ownership_card, ownership_id, please_parse = TRUE) {
  if (length(ownership_id) == 0) {
    stop("Please provide a valid ownership_id.")
  } else if (length(ownership_id) == 1) {
    # Lookup the cards for a single id
    return(lookupEntityCard(entity_card = ownership_card, entity_id = ownership_id, entity_path = "ownerships"))
  } else {
    # Add duplicate and time check
    duplicateTimeCheck(ownership_id)

    # Check please_parse
    if (please_parse) {
      # Bind data into a final data.frame with those elements without data dropping out
      return( rbind_pages(silenceFun(lapply(ownership_id, lookupEntityCard, entity_card = ownership_card, entity_path = "ownerships"))) )
    } else {
      # Return a list of (potentially empty) data.frames
      return(silenceFun(lapply(ownership_id, lookupEntityCard, entity_card = ownership_card, entity_path = "ownerships")))
    }
  }
}