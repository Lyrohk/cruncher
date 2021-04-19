#' Function to lookup multiple categories over the Entity Lookup API Endpoint
#'
#'@param persons UUID or permalink of the categories you wish to look up
#'@param please_parse TRUE or FALSE. By default TRUE. If set to FALSE, it will return the data directly from the JSON, if set to TRUE, it will parse it into a data.frame object
#'@return either a data.frame (if please_parse = TRUE) or a list of lists (if please_parse = FALSE)
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' lookUpCategories(c("	f9b14a15-5517-8f38-0562-729ebb54dfdb", "e8804a82-8603-d2e1-c3b5-d241dd8030b9"))
#' lookUpCategories(list("	f9b14a15-5517-8f38-0562-729ebb54dfdb", "e8804a82-8603-d2e1-c3b5-d241dd8030b9"))
#'
#' @import dplyr
#' @export
#'
# Lookup multiple category groups
lookupCategories <- function(categories, please_parse = TRUE) {
  return(lookupEntities(entities = categories, path = "categories", please_parse = please_parse))
}