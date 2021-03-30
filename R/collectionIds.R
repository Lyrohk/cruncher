# Helper function to view all collection ids possible
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
  
  # Print this list
  print(collectionids)
}