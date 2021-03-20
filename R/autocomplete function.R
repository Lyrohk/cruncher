# Autocomplete function
# Query: What are you looking for e.g. "Artificial Intelligence" category uuids.
# Collection Ids: Within which entities should we look for matches? e.g. categories, category_groups
autocomplete <- function(collection_ids = "organizations,people,funding_rounds,acquisitions,investments,events,press_references,funds,event_appearances,ipos,ownerships,categories,category_groups,locations,jobs",query,limit = 10) {
  
  
  # Check bounds of limit
  if (limit > 25 | limit < 1) {
    stop("Limit must be between 1 and 25. Please try again within this range.")
  }
  # Check that query exists
  if (query == "") { stop("Query is a required parameter.") }
  
  # # String manipulations of query
  query <- query %>%
    str_to_lower() %>%
    str_replace_all(" ", "%20")
  
  # Now make the Autocomplete API call
  url <-
    paste0(
      "https://api.crunchbase.com/api/v4/autocompletes",
      "?user_key=",
      API_KEY,
      "&collection_ids=",
      collection_ids,
      "&query=",
      query, 
      "&limit=",
      limit
    )
  response <- GET(url)
  
  # Check that it worked
  if (response$status_code == 200) {
    # Return data from JSON
    data <- fromJSON(rawToChar(response$content))
    
    # Return this df
    df <- data[["entities"]]
  }
}