#'Function to search for organizations that fulfill pre-specified search conditions
#'
#'@param path Type of entity e.g. organizations
#'@param card card field of interest that will be returned. Only one please!
#'@param parse TRUE or FALSE. By default TRUE. If set to FALSE, it will return the data directly from the JSON, if set to TRUE, it will parse it into a data.frame object
#'@return either a data.frame (if parse = TRUE) or a list (if parse = FALSE)
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' lookUpDetailForOrganization(id = "facebook", card = "investors")
#'
#' @import httr
#' @import jsonlite
#' @import stringr
#' @import dplyr
#'
searchforOrganizations <- function(conditions, order_by = "value", sort_by = "asc") {




}