#'Function to lookup multiple press references over the Entity Lookup API Endpoint
#'
#'@param press_references UUID or permalink of the press references you wish to look up
#'@param please_parse TRUE or FALSE. By default TRUE. If set to FALSE, it will return the data directly from the JSON, if set to TRUE, it will parse it into a data.frame object
#'@return either a data.frame (if please_parse = TRUE) or a list of lists (if please_parse = FALSE)
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' lookupPressReferences(c("uuid1", "uuid2"))
#' lookupPressReferences(list("uuid1", "uuid2"))
#'
#' @import dplyr
#' @export
#'
lookupPressReferences <- function(press_references, please_parse = TRUE) {
  return(lookupEntities(entities = press_references, path = "press_references", please_parse = please_parse))
}