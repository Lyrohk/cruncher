#' Function to lookup a single organization over the Entity Lookup API Endpoint
#'
#'@param organizations UUID or permalink of the organizations you wish to look up
#'@param please_parse TRUE or FALSE. By default TRUE. If set to FALSE, it will return the data directly from the JSON, if set to TRUE, it will parse it into a data.frame object
#'@return either a data.frame (if please_parse = TRUE) or a list of lists (if please_parse = FALSE)
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' lookUpOrganizations(c("facebook", "google"))
#' lookUpOrganizations(list("facebook", "google"))
#'
#' @import dplyr
#' @export
#'
# Lookup multiple organizations
lookupOrganizations <- function(organizations, please_parse = TRUE) {
  return(lookupEntities(entities = organizations, path = "organizations", please_parse = please_parse))
}