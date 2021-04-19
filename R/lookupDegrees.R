#' Function to lookup multiple degrees over the Entity Lookup API Endpoint
#'
#'@param persons UUID or permalink of the degrees you wish to look up
#'@param please_parse TRUE or FALSE. By default TRUE. If set to FALSE, it will return the data directly from the JSON, if set to TRUE, it will parse it into a data.frame object
#'@return either a data.frame (if please_parse = TRUE) or a list of lists (if please_parse = FALSE)
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' lookupDegrees(c("uuid1", "uuid2"))
#' lookupDegrees(list("uuid1", "uuid2"))
#'
#' @import dplyr
#' @export
#'
# Lookup multiple degrees
lookupDegrees <- function(degrees, please_parse = TRUE) {
  return(lookupEntities(entities = degrees, path = "degrees", please_parse = please_parse))
}