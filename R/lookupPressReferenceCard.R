#' Function to lookup and paginate through a single card for a single press reference over the Entity Lookup API Endpoint
#'
#' Takes the entity id to lookup a card and paginate through the entities and parse them into a data.frame
#'
#'@param press_reference_card card field of interest that will be returned. Only one please!
#'@param press_reference_id UUID or permalink of the press reference you wish to look up
#'@param please_parse Logical. By default TRUE and will parse your data from a list of data.frames to a final data.frame with empty elements dropping out.
#'@return a data.frame
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' lookupPressReferenceCard(press_reference_card = "card", press_reference_id = "id")
#'
#'@export
#'
lookupPressReferenceCard <- function(press_reference_card, press_reference_id, please_parse = TRUE) {
  if (length(press_reference_id) == 0) {
    stop("Please provide a valid press_reference_id.")
  } else if (length(press_reference_id) == 1) {
    # Lookup the cards for a single id
    return(lookupEntityCard(entity_card = press_reference_card, entity_id = press_reference_id, entity_path = "press_references"))
  } else {
    # Add duplicate and time check
    duplicateTimeCheck(press_reference_id)

    # Check please_parse
    if (please_parse) {
      # Bind data into a final data.frame with those elements without data dropping out
      return( rbind_pages(silenceFun(lapply(press_reference_id, lookupEntityCard, entity_card = press_reference_card, entity_path = "press_references"))) )
    } else {
      # Return a list of (potentially empty) data.frames
      return(silenceFun(lapply(press_reference_id, lookupEntityCard, entity_card = press_reference_card, entity_path = "press_references")))
    }
  }
}