#' Function to lookup and paginate through a single card for a single person over the Entity Lookup API Endpoint
#'
#' Takes the entity id to lookup a card and paginate through the entities and parse them into a data.frame
#'
#'@param person_card card field of interest that will be returned. Only one please!
#'@param person_id UUID or permalink of the person you wish to look up
#'@return a data.frame
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' lookupPersonCard(person_card = "degrees", person_id = "mark-zuckerberg")
#'
#'@export
#'
lookupPersonCard <- function(person_card, person_id) {
  if (length(person_id) == 0) {
    stop("Please provide a valid person_id.")
  } else if (length(person_id) == 1) {
    # Lookup the cards for a single id
    return(lookupEntityCard(entity_card = person_card, entity_id = person_id, entity_path = "people"))
  } else {
    # There are multiple ids
    return(lapply(person_id, lookupEntityCard, entity_card = person_card, entity_path = "people"))
  }
}