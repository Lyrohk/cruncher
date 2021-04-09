#' Helper function to return all possible cards for an organization
#'
#'@return all cards for an organization. Remember to only pick one for the detailed Lookup function.
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' getOrganizationCards()
#'
#' @export
#'
# Function
getOrganizationCards <- function() {
  print(c("acquiree_acquisitions", 
                  "acquirer_acquisitions", 
                  "child_organizations",
                  "event_appearances",
                  "founders",
                  "headquarters_address",
                  "investors",
                  "ipos",
                  "jobs",
                  "parent_organization",
                  "parent_ownership",
                  "participated_funding_rounds",
                  "participated_funds",
                  "participated_investments",
                  "press_references",
                  "raised_funding_rounds",
                  "raised_funds",
                  "raised_investments"))
}