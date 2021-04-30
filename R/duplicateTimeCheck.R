#' duplicateTimeCheck
#'
#' Helper function to remove duplicates in the inputs (uuids/permalinks) of any lookupXXXCard() function and to inform the user of the approximate time the funciton will take to crunch the data.
#'
#'@param entities the uuid/permalink(s) of the entity to lookup a card for
#'@return a custom message that is printed to the console
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' cardPaginationCheck(c(uuid1, uuid2))
#'
#'
#'
duplicateTimeCheck <- function(entities) {

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
    cat(paste("The uuids or permalinks contained", original_length - length(entities), "duplicate(s). Duplicates are removed for efficiency purposes.\n"))
  }

  # Approximation of how long it is expect to take to retrieve the information
  approx_duration_min = length(entities)/ 150;
  # Print out approximate duration
  cat(paste("Expect this process to take around", approx_duration_min, "minute(s) and wait for cruncher to finish crunching...\n"))


}