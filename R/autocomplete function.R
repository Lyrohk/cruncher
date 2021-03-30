#' Function to use the Autocomplete API endpoint of Crunchbase API
#'
#'@param collection_ids Within which path the matching to the query should be performed e.g. organizations
#'@param query What we are looking for e.g. "Europe" in locations, "Artificial Intelligence" in categories
#'@param limit Number of entries to be returned at most (min 1, default 10, max 25)
#'@return the entities matching the query within the specified collection_ids and with the limit specified
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' autocomplete(collection_ids = "categories", query = "Artificial Intelligence", limit = 5)
#'
#' @import httr
#' @import jsonlite
#' @import stringr
#' @import dplyr
#' @export
#'
# Autocomplete function
# Query: What are you looking for e.g. "Artificial Intelligence" category uuids.
# Collection Ids: Within which entities should we look for matches? e.g. categories, category_groups
autocomplete <-
  function(query,
           collection_ids = "organizations,people,funding_rounds,acquisitions,investments,events,press_references,funds,event_appearances,ipos,ownerships,categories,category_groups,locations,jobs",
           limit = 10) {
    # Check API Key
    API_KEY = Sys.getenv("API_KEY")
    if (API_KEY == "") {
      stop(
        "Please set your Crunchbase API Key with the setAPIKey(). Please note that the basic access is not sufficient."
      )
    }
    # Check bounds of limit
    if (limit > 25 | limit < 1) {
      stop("Limit must be between 1 and 25. Please try again within this range.")
    }
    # Check that query exists
    if (query == "") {
      stop("Query is a required parameter.")
    }
    
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
      
      # Print df
      print(data[["entities"]])
      
      # Return this df
      df <- data[["entities"]]
    } else {
      # Return error code
      print(response$status_code)
    }
  }