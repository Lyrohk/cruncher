#' Search for entities
#' 
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' entities_uuids <- search(path = "ipos", json_list = predefinedBody)
#'
#' @import httr
#' @import jsonlite
#' @export
#'
search_entities <- function(path = "organizations", json_list) {
  
  # Check API Key
  API_KEY = Sys.getenv("API_KEY")
  if (API_KEY == "") {
    stop(
      "Please set your Crunchbase API Key with the setAPIKey(). Please note that the basic access is not sufficient."
    )
  }
  
  # Construct request url
  request_url <- paste0("https://api.crunchbase.com/api/v4/searches/", path, "?user_key=", API_KEY)
  
  # Get response
  response <- RETRY(verb = "POST", url = request_url, body = json_list, encode = "json")
  
  # Check status code
  if (response$status_code == 200) {
    # All good, continue on
    # Get data from JSON response
    data <- fromJSON(rawToChar(response$content))
    
    # Get total count of entities that were found
    total <- data$count
    
    # Print out how many counts we have
    print(paste("The Search Result has ", total, " results. Please wait while the results are being fetched."))
    
    # TODO Parse df
    df <- data.frame(uuid = data$entities$uuid) 
    
    # Get number of entities returned
    check <- NROW(df)
    print(paste("Currently fetched results amount to", check))
    
    # If there are more entities than count, fetch all
    while (check < total) { 
      # Put last uuid as after_id parameter to the body
      json_list$after_id <- tail(data$entities$uuid, 1)
      
      # Get response
      response <- RETRY(verb = "POST", url = request_url, body = json_list, encode = "json")
      
      # Check status code
      if (response$status_code == 200) {
        # Read out data 
        data <- fromJSON(rawToChar(response$content))
        
        # TODO Read out data 
        next_df <- data.frame(uuid = data$entities$uuid) #data$entities$properties
        
        # Combine into one
        df <- rbind(df, next_df)
        
        
        # Get number of entities returned
        check <- NROW(df)
        print(paste("Fetched results now", check, "and counting..."))
        
      } else {
        # Return error
        print(response$status_code)
      }
    }
    
    # Return dataframe
    return(df)
  } else {
    # Return error
    print(response$status_code)
  }
}