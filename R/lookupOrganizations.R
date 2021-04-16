#' Function to lookup a single organization over the Entity Lookup API Endpoint
#'
#'@param organizations UUID or permalink of the organizations you wish to look up
#'@param please_parse TRUE or FALSE. By default TRUE. If set to FALSE, it will return the data directly from the JSON, if set to TRUE, it will parse it into a data.frame object
#'@return either a data.frame (if please_parse = TRUE) or a list of lists (if please_parse = FALSE)
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' lookUpOrganizations(c("facebook", "google"))
#' lookUpOrganizations(list("facebook", "google"))
#'
#' @import dplyr
#' @export
#'
# Lookup multiple organizations
lookUpOrganizations <- function(organizations, please_parse = TRUE) {
  # Check organizations for class type
  if (!class(organizations) %in% c("list", "character")) {
    stop("Please ensure that the organizations are either a character vector or a list of character elements.")
  }
  
  # Check logical
  if (!class(please_parse) == "logical") {
    stop("Please use a TRUE or FALSE as input for please_parse.")
  }
  
  # Return requested output
  if (please_parse) {
    # Return a dataframe with each row having all the information about a certain organization
    return(do.call(rbind.data.frame, lapply(X = organizations, FUN = lookUpOrganization)))
  } else {
    # Return the data converted from json as lists
    return(do.call(list, lapply(X = organizations, FUN = lookUpOrganization, please_parse = FALSE)))
  }
}