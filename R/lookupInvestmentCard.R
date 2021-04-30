#' Function to lookup and paginate through a single card for a single investment over the Entity Lookup API Endpoint
#'
#' Takes the entity id to lookup a card and paginate through the entities and parse them into a data.frame
#'
#'@param investment_card card field of interest that will be returned. Only one please!
#'@param investment_id UUID or permalink of the investment you wish to look up
#'@return a data.frame
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' lookupInvestmentCard(investment_card = "degrees", investment_id = "mark-zuckerberg")
#'
#'@export
#'
lookupInvestmentCard <- function(investment_card, investment_id) {
  if (length(investment_id) == 0) {
    stop("Please provide a valid investment_id.")
  } else if (length(investment_id) == 1) {
    # Lookup the cards for a single id
    return(lookupEntityCard(entity_card = investment_card, entity_id = investment_id, entity_path = "investments"))
  } else {
    # Add duplicate and time check
    duplicateTimeCheck(investment_id)

    # There are multiple ids
    return(silenceFun(lapply(investment_id, lookupEntityCard, entity_card = investment_card, entity_path = "investments")))
  }
}