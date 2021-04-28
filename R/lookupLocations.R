#' Function to lookup multiple locations over the Entity Lookup API Endpoint
#'
#'@param persons UUID or permalink of the locations you wish to look up
#'@param please_parse Logical. By default TRUE. If set to FALSE, it will return the data directly from the JSON, if set to TRUE, it will parse it into a data.frame object
#'@param print_error Logical. Default to TRUE and thus if the request fails, an error message will be printed out.
#'@return either a data.frame (if please_parse = TRUE) or a list of lists (if please_parse = FALSE)
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' lookupLocations(c("uuid1", "uuid2"))
#' lookupLocations(list("uuid1", "uuid2"))
#'
#' @import dplyr
#' @export
#'
# Lookup multiple locations
lookupLocations <- function(locations, please_parse = TRUE, print_error = TRUE) {
  return(lookupEntities(entities = locations, path = "locations", please_parse = please_parse, print_error = print_error))
}