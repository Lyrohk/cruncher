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
  # Print paths as list
  paths <- vector(mode = "list", length = 15)
  paths[[1]] <- "organizations"
  paths[[2]] <- "people"
  paths[[3]] <- "funding_rounds"
  paths[[4]] <- "acquisitions"
  paths[[5]] <- "investments"
  paths[[6]] <- "events"
  paths[[7]] <- "press_references"
  paths[[8]] <- "funds"
  paths[[9]] <- "event_appearances"
  paths[[10]] <- "ipos"
  paths[[11]] <- "ownerships"
  paths[[12]] <- "categories"
  paths[[13]] <- "category_groups"
  paths[[14]] <- "locations"
  paths[[15]] <- "jobs"
  
  # Print out 
  return(paths)
}