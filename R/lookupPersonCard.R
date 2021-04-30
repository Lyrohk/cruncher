#' Function to lookup and paginate through a single card for a single person over the Entity Lookup API Endpoint
#'
#' Takes the entity id to lookup a card and paginate through the entities and parse them into a data.frame
#'
#'@param person_card card field of interest that will be returned. Only one please!
#'@param person_id UUID or permalink of the person you wish to look up
#'@param please_parse Logical. By default TRUE and will parse your data from a list of data.frames to a final data.frame with empty elements dropping out.
#'@return a data.frame
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' lookupPersonCard(person_card = "degrees", person_id = "mark-zuckerberg")
#'
#'@export
#'
lookupPersonCard <- function(person_card, person_id, please_parse = TRUE) {
  if (length(person_id) == 0) {
    stop("Please provide a valid person_id.")
  } else if (length(person_id) == 1) {
    # Lookup the cards for a single id
    return(lookupEntityCard(entity_card = person_card, entity_id = person_id, entity_path = "people"))
  } else {
    # Add duplicate and time check
    duplicateTimeCheck(person_id)

    # Check please_parse
    if (please_parse) {
      # Bind data into a final data.frame with those elements without data dropping out
      return( rbind_pages(silenceFun(lapply(person_id, lookupEntityCard, entity_card = person_card, entity_path = "people"))) )
    } else {
      # Return a list of (potentially empty) data.frames
      return(silenceFun(lapply(person_id, lookupEntityCard, entity_card = person_card, entity_path = "people")))
    }
  }
}