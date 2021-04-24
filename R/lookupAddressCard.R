#' Function to lookup and paginate through a single card for a single address over the Entity Lookup API Endpoint
#'
#' Takes the entity id to lookup a card and paginate through the entities and parse them into a data.frame
#'
#'@param address_card card field of interest that will be returned. Only one please!
#'@param address_id UUID or permalink of the address you wish to look up
#'@return a data.frame
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' lookupAddressCard(address_card = "card", address_id = "id")
#'
#'@export
#'
lookupAddressCard <- function(address_card, address_id) {
  return(lookupEntityCard(entity_card = address_card, entity_id = address_id, entity_path = "addresses"))
}