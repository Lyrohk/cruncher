#' Helper function to return the correct parsing function for a given card
#'
#'@param card that is of interest
#'@return the appropriate parser
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' getParserForCard("founders")
#'
#'
getParserForCard <- function(card) {
  if (card %in% c("organization", "child_organizations", "child_organization", "parent_organization", "founded_organizations", "primary_organization", "acquiree_organization", "acquirer_organization", "owner")) {
    return(parseOrganization)
  } else if (card %in% c("person", "founders", "partners", "partner")) {
    return(parsePerson)
  } else if (card %in% c("acquiree_acquisitions", "acquirer_acquisitions")) {
    return(parseAquisition)
  } else if (card %in% c("investments", "raised_investments", "participated_investments", "partner_investments")) {
    return(parseInvestment)
  } else if (card %in% c("event")) {
    return(parseEvent)
  } else if (card %in% c("event_appearances", "appearances")) {
    return(parseEventAppearance)
  } else if (card %in% c("participated_funds", "raised_funds")) {
    return(parseFund)
  } else if (card %in% c("funding_round", "raised_funding_rounds", "participated_funding_rounds", "partner_funding_rounds")) {
    return(parseFundingRound)
  } else if (card %in% c("ipos")) {
    return(parseIpo)
  } else if (card %in% c("jobs", "primary_job")) {
    return(parseJob)
  } else if (card %in% c("degrees")) {
    return(parseDegree)
  } else if (card %in% c("press_references")) {
    return(parsePressReference)
  } else if (card %in% c("parent_ownership", "child_ownerships")) {
    return(parseOwnership)
  } else if (card %in% c("address", "headquarters_address")) {
    return(parseAddress)
  } else if (card %in% c("investors", "lead_investors", "exhibitors", "speakers", "contestants", "sponsors", "participant")) {
    # Note: Parse Principal is used when it is unclear whether the subject is a person or another type of entity. Think of it as a combination of organization and person parsing
    return(parsePrincipal)
  } else {
    # card was not recognized, stop here
    stop(paste0("Card ", card, " was not recognized in getParserForCard function. Please check your spelling before trying again."))
  }
}