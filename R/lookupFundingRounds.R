#' Function to lookup a multiple funding rounds over the Entity Lookup API Endpoint
#'
#'@param funding_rounds UUID or permalink of the funding rounds you wish to look up
#'@param please_parse TRUE or FALSE. By default TRUE. If set to FALSE, it will return the data directly from the JSON, if set to TRUE, it will parse it into a data.frame object
#'@return either a data.frame (if please_parse = TRUE) or a list of lists (if please_parse = FALSE)
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' lookupFundingRounds(c("8a945939-18e0-cc9d-27b9-bf33817b2818", "d950d7a5-79ff-fb93-ca87-13386b0e2feb"))
#' lookupFundingRounds(list("8a945939-18e0-cc9d-27b9-bf33817b2818", "d950d7a5-79ff-fb93-ca87-13386b0e2feb"))
#'
#' @import dplyr
#' @export
#'
# Lookup multiple funding rounds
lookupFundingRounds <- function(funding_rounds, please_parse = TRUE) {
  return(lookupEntities(entities = funding_rounds, path = "funding_rounds", please_parse = please_parse))
}