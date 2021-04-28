#' Function to use the Autocomplete API endpoint of Crunchbase API
#'
#'@param search_for search_for we are looking for e.g. "Europe" in locations, "Artificial Intelligence" in categories
#'@param within A comma separated list of collection ids to search against e.g. "categories,category_groups". Defaults to all.
#'@param limit Number of entries to be returned at most (min 1, default 10, max 25)
#'@return the entities matching the search_for within the specified within and with the limit specified
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' autocomplete(search_for = "Artificial Intelligence", within = "categories", limit = 5)
#'
#' @import httr
#' @import jsonlite
#' @import stringr
#' @import dplyr
#' @export
#'
# Autocomplete function
# search_for: What are you looking for e.g. "Artificial Intelligence" category uuids.
# Collection Ids: Within which entities should we look for matches? e.g. categories, category_groups
autocomplete <-
  function(search_for,
           within = "",
           limit = 10) {
    #Check API Key
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

    # Check that search_for exists
    if (search_for == "") {
      stop("search_for is a required parameter.")
    }

    # # String manipulations of search_for
    search_for <- search_for %>%
      str_to_lower() %>%
      str_replace_all(" ", "%20")

    # Check if within are specified
    url <- ifelse(within == "",
           paste0(
             "https://api.crunchbase.com/api/v4/autocompletes",
             "?user_key=",
             API_KEY,
             "&query=",
             search_for,
             "&limit=",
             limit
           ),
           paste0(
             "https://api.crunchbase.com/api/v4/autocompletes",
             "?user_key=",
             API_KEY,
             "&collection_ids=",
             within,
             "&query=",
             search_for,
             "&limit=",
             limit
           ))

    # Now make the Autocomplete API call
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
      printError(response$status_code)
    }
  }