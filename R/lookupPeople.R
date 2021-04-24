#' Function to lookup a multiple people over the Entity Lookup API Endpoint
#'
#'@param persons UUID or permalink of the people you wish to look up
#'@param please_parse TRUE or FALSE. By default TRUE. If set to FALSE, it will return the data directly from the JSON, if set to TRUE, it will parse it into a data.frame object
#'@return either a data.frame (if please_parse = TRUE) or a list of lists (if please_parse = FALSE)
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' lookUpPersons(c("mark-zuckerberg", "roland-berger"))
#' lookUpPersons(list("mark-zuckerberg", "roland-berger"))
#'
#' @import dplyr
#' @export
#'
# Lookup multiple organizations
lookupPeople <- function(persons, please_parse = TRUE) {
  return(lookupEntities(entities = persons, path = "people", please_parse = please_parse))
}