#' Function to lookup and paginate through a single card for a single organization over the Entity Lookup API Endpoint
#'
#' Takes the entity id to lookup a card and paginate through the entities and parse them into a data.frame
#'
#'@param organization_card card field of interest that will be returned. Only one please!
#'@param organization_id UUID or permalink of the organization you wish to look up
#'@return a data.frame
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' lookupOrganizationCard(organization_card = "investors", organization_id = "facebook")
#'
#'@export
#'
lookupOrganizationCard <- function(organization_card, organization_id) {
  return(lookupEntityCard(entity_card = organization_card, entity_id = organization_id, entity_path = "organizations"))
}