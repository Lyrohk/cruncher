#' Function to lookup and paginate through a single card for a single funding round over the Entity Lookup API Endpoint
#'
#' Takes the entity id to lookup a card and paginate through the entities and parse them into a data.frame
#'
#'@param entity_card card field of interest that will be returned. Only one please!
#'@param entity_id UUID or permalink of the funding round you wish to look up
#'@return a data.frame
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' lookupFundingRoundCard(funding_round_card = "degrees", funding_round_id = "mark-zuckerberg")
#'
#'@export
#'
lookupFundingRoundCard <- function(funding_round_card, funding_round_id) {
  return(lookupEntityCard(entity_card = funding_round_card, entity_id = funding_round_id, entity_path = "funding_rounds"))
}