#' Function to lookup and paginate through a single card for a single acquisition over the Entity Lookup API Endpoint
#'
#' Takes the entity id to lookup a card and paginate through the entities and parse them into a data.frame
#'
#'@param acquisition_card card field of interest that will be returned. Only one please!
#'@param acquisition_id UUID or permalink of the acquisition you wish to look up
#'@return a data.frame
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' lookupAcquisitionCard(acquisition_card = "degrees", acquisition_id = "mark-zuckerberg")
#'
#'@export
#'
lookupAcquisitionCard <- function(acquisition_card, acquisition_id) {
  return(lookupEntityCard(entity_card = acquisition_card, entity_id = acquisition_id, entity_path = "acquisitions"))
}