#' Function to lookup a single event over the Entity Lookup API Endpoint
#'
#'@param id UUID or permalink of the event you wish to look up
#'@param parse TRUE or FALSE. By default TRUE. If set to FALSE, it will return the data directly from the JSON, if set to TRUE, it will parse it into a data.frame object
#'@return either a data.frame (if parse = TRUE) or a list (if parse = FALSE)
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' lookUpEvent("uuid")
#'
#' @import httr
#' @import jsonlite
#' @export
#'
# Call the function to get you information about an event
lookupEvent <- function(id, please_parse = TRUE)  {
  return(lookupEntity(id = id, path = "events", please_parse = please_parse))
}