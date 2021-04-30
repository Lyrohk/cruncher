#' Function to lookup and paginate through a single card for a single ipo over the Entity Lookup API Endpoint
#'
#' Takes the entity id to lookup a card and paginate through the entities and parse them into a data.frame
#'
#'@param ipo_card card field of interest that will be returned. Only one please!
#'@param ipo_id UUID or permalink of the ipo you wish to look up
#'@param please_parse Logical. By default TRUE and will parse your data from a list of data.frames to a final data.frame with empty elements dropping out.
#'@return a data.frame
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' lookupIpoCard(ipo_card = "card", ipo_id = "id")
#'
#'@export
#'
lookupIpoCard <- function(ipo_card, ipo_id, please_parse = TRUE) {
  if (length(ipo_id) == 0) {
    stop("Please provide a valid ipo_id.")
  } else if (length(ipo_id) == 1) {
    # Lookup the cards for a single id
    return(lookupEntityCard(entity_card = ipo_card, entity_id = ipo_id, entity_path = "ipos"))
  } else {
    # Add duplicate and time check
    duplicateTimeCheck(ipo_id)

    # Check please_parse
    if (please_parse) {
      # Bind data into a final data.frame with those elements without data dropping out
      return( rbind_pages(silenceFun(lapply(ipo_id, lookupEntityCard, entity_card = ipo_card, entity_path = "ipos"))) )
    } else {
      # Return a list of (potentially empty) data.frames
      return(silenceFun(lapply(ipo_id, lookupEntityCard, entity_card = ipo_card, entity_path = "ipos")))
    }
  }
}