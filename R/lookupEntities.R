#' Function to lookup multiple entities over the Entity Lookup API Endpoint
#'
#'@param entities UUID or permalink of the entities you wish to look up
#'@param path of the entity e.g. organizations, people, etc.
#'@param please_parse TRUE or FALSE. By default TRUE. If set to FALSE, it will return the data directly from the JSON, if set to TRUE, it will parse it into a data.frame object
#'@return either a data.frame (if please_parse = TRUE) or a list of lists (if please_parse = FALSE)
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#'
#' @import dplyr
#'
#' @export
#'
# Lookup multiple entities
lookupEntities <- function(entities, path, please_parse = TRUE) {
  # Check entities for class type
  if (!class(entities) %in% c("list", "character")) {
    stop("Please ensure that the entities are either a character vector or a list of character elements.")
  }

  # Check logical
  if (!class(please_parse) == "logical") {
    stop("Please use a TRUE or FALSE as input for please_parse.")
  }

  # Check that path is specified
  if (missing(path)) {
    stop("Please specify the path where the entity can be found e.g. 'organizations'.")
  }
  # Depending on path, choose the right functions
  if (path == "organizations") {
    my_fun <- lookupOrganization
  } else if (path == "people") {
    my_fun <- lookupPerson
  } else if (path == "acquisitions") {
    my_fun <- lookupAquisition
  } else if (path == "investments") {
    my_fun <- lookupInvestment
  } else if (path == "events") {
    my_fun <- lookupEvent
  } else if (path == "event_appearances") {
    my_fun <- lookupEventAppearance
  } else if (path == "funds") {
    my_fun <- lookupFund
  } else if (path == "funding_rounds") {
    my_fun <- lookupFundingRound
  } else if (path == "ipos") {
    my_fun <- lookupIpo
  } else if (path == "jobs") {
    my_fun <- lookupJob
  } else if (path == "degrees") {
    my_fun <- lookupDegree
  } else if (path == "press_references") {
    my_fun <- lookupPressReference
  } else if (path == "categories") {
    my_fun <- lookupCategory
  } else if (path == "category_groups") {
    my_fun <- lookupCategoryGroup
  } else if (path == "ownerships") {
    my_fun <- lookupOwnership
  } else if (path == "locations") {
    my_fun <- lookupLocation
  } else {
    # Path was not recognized, stop here
    stop("Path was not recognized. Please check your spelling before trying again.")
  }

  # Return requested output
  if (please_parse) {
    # Return a dataframe with each row having all the information about a certain entities
    df <- do.call(rbind.data.frame, lapply(X = entities, FUN = my_fun))
    # Filter out NA columns
    df <- df[colSums(!is.na(df)) > 0]
    # Return as data.frame
    return(df)
  } else {
    # Return the data converted from json as lists
    return(do.call(list, lapply(X = entities, FUN = my_fun, please_parse = FALSE)))
  }
}