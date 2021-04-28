#' Function to lookup multiple ipos over the Entity Lookup API Endpoint
#'
#'@param ipos UUID or permalink of the ipos you wish to look up
#'@param please_parse Logical. By default TRUE. If set to FALSE, it will return the data directly from the JSON, if set to TRUE, it will parse it into a data.frame object
#'@param print_error Logical. Default to TRUE and thus if the request fails, an error message will be printed out.
#'@return either a data.frame (if parse = TRUE) or a list (if parse = FALSE)
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' lookUpIpos(c("uuid1", "uuid2"))
#'
#' @import httr
#' @import jsonlite
#' @export
#'
# Call the function to get you information about an ipo
lookupIpos <- function(ipos, please_parse = TRUE, print_error = TRUE)  {
  return(lookupEntities(entities = ipos, path = "ipos", please_parse = please_parse, print_error = print_error))
}