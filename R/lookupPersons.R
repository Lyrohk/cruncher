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
lookUpPersons <- function(persons, please_parse = TRUE) {
  # Check organizations for class type
  if (!class(persons) %in% c("list", "character")) {
    stop("Please ensure that the organizations are either a character vector or a list of character elements.")
  }
  
  # Check logical
  if (!class(please_parse) == "logical") {
    stop("Please use a TRUE or FALSE as input for please_parse.")
  }
  
  # Return requested output
  if (please_parse) {
    # Return a dataframe with each row having all the information about a certain organization
    return(do.call(rbind.data.frame, lapply(X = persons, FUN = lookUpPerson)))
  } else {
    # Return the data converted from json as lists
    return(do.call(list, lapply(X = persons, FUN = lookUpPerson, please_parse = FALSE)))
  }
}