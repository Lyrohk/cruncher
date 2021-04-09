#' Helper function to return all colleciton ids
#'
#'@return all valid collection ids
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' getCollectionIds()
#'
#' @export
#'
# Function
getCollectionIds <- function() {
  # Make list of all collection ids
  collectionids <- vector(mode = "list", length = 15)
  collectionids[[1]] <- "organizations"
  collectionids[[2]] <- "people"
  collectionids[[3]] <- "funding_rounds"
  collectionids[[4]] <- "acquisitions"
  collectionids[[5]] <- "investments"
  collectionids[[6]] <- "events"
  collectionids[[7]] <- "press_references"
  collectionids[[8]] <- "funds"
  collectionids[[9]] <- "event_appearances"
  collectionids[[10]] <- "ipos"
  collectionids[[11]] <- "ownerships"
  collectionids[[12]] <- "categories"
  collectionids[[13]] <- "category_groups"
  collectionids[[14]] <- "locations"
  collectionids[[15]] <- "jobs"
  
  # Print out info
  print("The following are the possible inputs for collection ids (same as paths):")
  print("organizations, people, funding_rounds, acquisitions, investments, events, press_references, funds, event_appearances, ipos, ownerships, categories, category_groups, locations, and jobs.")
  print("If you want to include more than one, please put them into a vector first like so:")
  print("my_vector <- c('organizations,people')")
}