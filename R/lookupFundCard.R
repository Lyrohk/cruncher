#' Function to lookup and paginate through a single card for a single fund over the Entity Lookup API Endpoint
#'
#' Takes the entity id to lookup a card and paginate through the entities and parse them into a data.frame
#'
#'@param fund_card card field of interest that will be returned. Only one please!
#'@param fund_id UUID or permalink of the fund you wish to look up
#'@return a data.frame
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' lookupFundCard(fund_card = "card", fund_id = "id")
#'
#'@export
#'
lookupFundCard <- function(fund_card, fund_id) {
  if (length(fund_id) == 0) {
    stop("Please provide a valid fund_id.")
  } else if (length(fund_id) == 1) {
    # Lookup the cards for a single id
    return(lookupEntityCard(entity_card = fund_card, entity_id = fund_id, entity_path = "funds"))
  } else {
    # Add duplicate and time check
    duplicateTimeCheck(fund_id)

    # There are multiple ids
    return(silenceFun(lapply(fund_id, lookupEntityCard, entity_card = fund_card, entity_path = "funds")))
  }
}