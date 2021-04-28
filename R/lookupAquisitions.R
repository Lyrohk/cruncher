#' Function to lookup a multiple acquisitions over the Entity Lookup API Endpoint
#'
#'@param acquisitions UUID or permalink of the acquisitions you wish to look up
#'@param please_parse Logical. By default TRUE. If set to FALSE, it will return the data directly from the JSON, if set to TRUE, it will parse it into a data.frame object
#'@param print_error Logical. Default to TRUE and thus if the request fails, an error message will be printed out.
#'@return either a data.frame (if please_parse = TRUE) or a list of lists (if please_parse = FALSE)
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' lookupAquisitions(c("c62807f2-3487-f63b-e7fc-559b752ed44c", "419cabe1-4d71-5f64-3e9b-c9d0fb445cca"))
#' lookupAquisitions(list("c62807f2-3487-f63b-e7fc-559b752ed44c", "419cabe1-4d71-5f64-3e9b-c9d0fb445cca"))
#'
#' @import dplyr
#' @export
#'
# Lookup multiple acquisitions
lookupAquisitions <- function(acquisitions, please_parse = TRUE, print_error = TRUE) {
  return(lookupEntities(entities = acquisitions, path = "acquisitions", please_parse = please_parse, print_error = print_error))
}