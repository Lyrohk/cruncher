#' Function to lookup a single address over the Entity Lookup API Endpoint
#'
#'@param id UUID or permalink of the address you wish to look up
#'@param parse TRUE or FALSE. By default TRUE. If set to FALSE, it will return the data directly from the JSON, if set to TRUE, it will parse it into a data.frame object
#'@return either a data.frame (if parse = TRUE) or a list (if parse = FALSE)
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' lookupAddress("id")
#'
#' @import httr
#' @import jsonlite
#' @export
#'
# Call the function to get you information about a category entity
lookupAddress <- function(id, please_parse = TRUE)  {
  return(lookupEntity(id = id, path = "addresses", please_parse = please_parse))
}