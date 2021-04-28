#' Function to lookup multiple entities over the Entity Lookup API Endpoint
#'
#'@param entities UUID or permalink of the entities you wish to look up
#'@param path of the entity e.g. organizations, people, etc.
#'@param please_parse Logical. By default TRUE. If set to FALSE, it will return the data directly from the JSON, if set to TRUE, it will parse it into a data.frame object
#'@param print_error Logical. Default to TRUE and thus if the request fails, an error message will be printed out.
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
lookupEntities <- function(entities, path, please_parse = TRUE, print_error = TRUE) {

  # Check entities for class type
  if (!class(entities) %in% c("list", "character")) {
    stop("Please ensure that the entities are either a character vector or a list of character elements.")
  }

  # String check
  entities <- lapply(entities, entityIdCheck)

  # Check uniqueness
  original_length <- length(entities)
  entities <- unique(entities)
  if (original_length > length(entities)) {
    cat(paste("The uuids contained", original_length - length(entities), "duplicate(s). Duplicates are removed for efficiency purposes.\n"))
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
  } else if (path == "addresses") {
    my_fun <- lookupAddress
  } else {
    # Path was not recognized, stop here
    stop("Path was not recognized. Please check your spelling before trying again.")
  }

  # Approximation of how long it is expect to take to retrieve the information (50 pro 30 sek)
  approx_duration_min = length(entities)/ 100;
  # Print out approximate duration
  cat(paste("Expect this process to take around", approx_duration_min, "minute(s) and wait for cruncher to finish crunching...\n"))


  # Return requested output
  if (please_parse) {
    # Return a dataframe with each row having all the information about a certain entities
    df <- do.call(rbind.data.frame, lapply(X = entities, FUN = my_fun, print_error = print_error))
    # Filter out na columns
    return(df[colSums(!is.na(df)) > 0])
  } else {
    # Return the data converted from json as lists
    return(do.call(list, lapply(X = entities, FUN = my_fun, please_parse = FALSE, print_error = print_error)))
  }
}