#' Function to lookup a single event over the Entity Lookup API Endpoint
#'
#'@param id UUID or permalink of the event you wish to look up
#'@param parse TRUE or FALSE. By default TRUE. If set to FALSE, it will return the data directly from the JSON, if set to TRUE, it will parse it into a data.frame object
#'@return either a data.frame (if parse = TRUE) or a list (if parse = FALSE)
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' lookUpEvent("uuid")
#'
#' @import httr
#' @import jsonlite
#' @export
#'
# Call the function to get you information about an event
lookUpEvent <- function(id, please_parse = TRUE)  {
  
  # Check that API_KEY exists
  if (!exists("API_KEY")) {
    stop("Please set a valid user key to API_KEY as an environmental variable or for use setAPIKey() to do it for you.")
  }
  
  # Check that id has been specified 
  if (missing(id)) {
    stop("Please enter a uuid or permalink for the person you wish to look up. You can find them manually from the browser or csv files.")
  }
  
  # Entity id check e.g. to lower, trimming whitespace, and replacing spaces with -
  id <- entityIdCheck(id)
  
  # Create the path
  url <- paste0("https://api.crunchbase.com/api/v4/entities/events/", id, "?card_ids=fields&user_key=", API_KEY)
  
  # Make http GET request
  response <- RETRY(verb = "GET", url = url) # Could use GET but someone may apply it with a list
  
  # Check if we get valid data, if not return error core
  if (response$status_code == 200) {
    data <- fromJSON(rawToChar(response$content))
    # Check if parsing is wanted
    if (please_parse) {
      
      # Parse event to return a dataframe
      # Simple parsing
      uuid <- simpleParse(data[["properties"]][["identifier"]][["uuid"]])
      name <- simpleParse(data[["properties"]][["identifier"]][["value"]])
      short_description <- simpleParse(data[["cards"]][["fields"]][["short_description"]])
      description <- simpleParse(data[["cards"]][["fields"]][["description"]])
      starts_on <- simpleParse(data[["cards"]][["fields"]][["starts_on"]])
      ends_on <- simpleParse(data[["cards"]][["fields"]][["ends_on"]])
      num_sponsors <- simpleParse(data[["cards"]][["fields"]][["num_sponsors"]])
      event_type <- simpleParse(data[["cards"]][["fields"]][["event_type"]])
      num_speakers <- simpleParse(data[["cards"]][["fields"]][["num_speakers"]])
      rank_event <- simpleParse(data[["cards"]][["fields"]][["rank_event"]])
      num_exhibitors <- simpleParse(data[["cards"]][["fields"]][["num_exhibitors"]])
      image_url <- simpleParse(data[["cards"]][["fields"]][["image_url"]])
      created_at <- simpleParse(data[["cards"]][["fields"]][["created_at"]])
      updated_at <- simpleParse(data[["cards"]][["fields"]][["updated_at"]])
      
      # List parsing
      categories <- listParse(data[["cards"]][["fields"]][["categories"]])
      category_groups <- listParse(data[["cards"]][["fields"]][["category_groups"]])
      registration_url <- listParse(data[["cards"]][["fields"]][["registration_url"]])
      
      # Put into one dateframe
      df <- data.frame(cbind(  uuid ,
                               name,
                               short_description ,
                               description ,
                               starts_on ,
                               ends_on ,
                               event_type ,
                               rank_event ,
                               num_speakers ,
                               num_sponsors ,
                               num_exhibitors ,
                               image_url ,
                               created_at ,
                               updated_at, # Simple parsing done
                               categories ,
                               category_groups ,
                               registration_url 
                               
      ))
      
      # Adjust classes
      # as numeric
      df$rank_event <- as.numeric(df$rank_event)
      df$num_speakers <- as.numeric(df$num_speakers)
      df$num_sponsors <- as.numeric(df$num_sponsors)
      df$num_exhibitors <- as.numeric(df$num_exhibitors)
      
      # as date
      df$starts_on <- as.Date(df$starts_on)
      df$ends_on <- as.Date(df$ends_on)
      df$created_at <- as.Date(df$created_at)
      df$updated_at <- as.Date(df$updated_at)
      
      # Return dataframe
      df
      
    } else {
      # Return data
      return(data)
    }
  } else {
    # Print error code
    print(response$status_code)
  }
}