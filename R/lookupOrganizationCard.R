#' Function to lookup and paginate through a single card for a single organization over the Entity Lookup API Endpoint
#'
#' Takes the entity id to lookup a card and paginate through the entities and parse them into a data.frame
#'
#'@param organization_card card field of interest that will be returned. Only one please!
#'@param organization_id UUID or permalink of the organization you wish to look up
#'@param please_parse Logical. By default TRUE and will parse your data from a list of data.frames to a final data.frame with empty elements dropping out.
#'@return a data.frame
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' lookupOrganizationCard(organization_card = "investors", organization_id = "facebook")
#'
#'@export
#'
lookupOrganizationCard <- function(organization_card, organization_id, please_parse = TRUE) {
  if (length(organization_id) == 0) {
    stop("Please provide a valid organization_id.")
  } else if (length(organization_id) == 1) {
    # Lookup the cards for a single id
    return(lookupEntityCard(entity_card = organization_card, entity_id = organization_id, entity_path = "organizations"))
  } else {
    # Add duplicate and time check
    duplicateTimeCheck(organization_id)

    # Check please_parse
    if (please_parse) {
      # Bind data into a final data.frame with those elements without data dropping out
      return( rbind_pages(silenceFun(lapply(organization_id, lookupEntityCard, entity_card = organization_card, entity_path = "organizations"))) )
    } else {
      # Return a list of (potentially empty) data.frames
      return(silenceFun(lapply(organization_id, lookupEntityCard, entity_card = organization_card, entity_path = "organizations")))
    }
  }
}