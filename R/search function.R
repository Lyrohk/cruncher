#' Function to use the Search API endpoint of Crunchbase API
#'
#'@param path What are you searching for? Default: organizations. Other options include:  organizations
#'@param body The JSON body that specifies the query, order, limit etc.
#'@return JSON response
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' ipos_data <- search(path = "ipos", body = predefinedBody)
#'
#' @import httr
#' @import jsonlite
#' @export
#'
search <- function(path = "organizations", body) {
  
  # Check API Key
  API_KEY = Sys.getenv("API_KEY")
  if (API_KEY == "") {
    stop(
      "Please set your Crunchbase API Key with the setAPIKey(). Please note that the basic access is not sufficient."
    )
  }
  
  # Construct url
  url <- paste0("https://api.crunchbase.com/api/v4/searches/", path, "?user_key=", API_KEY)
  
  # Make POST request
  response <- httr::POST(url, 
                   body = jsonlite::toJSON(body), #This is your request json
                   encode = "json")
  
  if (response$status_code == 200) {
    # Parse it into readable content
    data <- fromJSON(rawToChar(response$content)) 
    
    # Return data
    data  
  } else {
    print(response$status_code)
  }
  
  
}