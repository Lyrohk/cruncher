#' Helper function to return all paths
#'
#'@return all valid paths
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' getPaths()
#'
#' @export
#'
# Helper function to view all paths possible
getPaths <- function() {
  # Paths as list
  paths <- c("organizations",
 "people",
"funding_rounds",
 "acquisitions",
"investments",
 "events",
"press_references",
"funds",
"event_appearances",
"ipos",
"ownerships",
"categories",
 "category_groups",
 "locations",
"jobs",
 "addresses")

  # Print out
  cat("Here are the available paths:\n")
  return(paths)
}