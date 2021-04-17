#' Function to lookup a single investment over the Entity Lookup API Endpoint
#'
#'@param id UUID or permalink of the investment you wish to look up
#'@param parse TRUE or FALSE. By default TRUE. If set to FALSE, it will return the data directly from the JSON, if set to TRUE, it will parse it into a data.frame object
#'@return either a data.frame (if parse = TRUE) or a list (if parse = FALSE)
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' lookUpInvestment("uuid")
#'
#' @import httr
#' @import jsonlite
#' @export
#'
# Call the function to get you information about an investment
lookUpInvestment <- function(id, please_parse = TRUE)  {
  
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
  url <- paste0("https://api.crunchbase.com/api/v4/entities/investments/", id, "?card_ids=fields&user_key=", API_KEY)
  
  # Make http GET request
  response <- RETRY(verb = "GET", url = url) # Could use GET but someone may apply it with a list
  
  # Check if we get valid data, if not return error core
  if (response$status_code == 200) {
    data <- fromJSON(rawToChar(response$content))
    # Check if parsing is wanted
    if (please_parse) {
      
      # Parse investment to return a dataframe
      # Simple parsing
      permalink <- simpleParse(data[["properties"]][["identifier"]][["permalink"]])
      uuid <- simpleParse(data[["properties"]][["identifier"]][["uuid"]])
      name <- simpleParse(data[["properties"]][["identifier"]][["value"]])
      # Field simple parsing
      announced_on <- simpleParse(data[["cards"]][["fields"]][["announced_on"]])
      investor_type <- simpleParse(data[["cards"]][["fields"]][["investor_type"]])
      funding_round_investment_type <- simpleParse(data[["cards"]][["fields"]][["funding_round_investment_type"]])
      is_lead_investor <- simpleParse(data[["cards"]][["fields"]][["is_lead_investor"]])
      created_at <- simpleParse(data[["cards"]][["fields"]][["created_at"]])
      updated_at <- simpleParse(data[["cards"]][["fields"]][["updated_at"]])
      
      # List parsing
      investor_identifier <- listParse(data[["cards"]][["fields"]][["investor_identifier"]])
      funding_round_identifier <- listParse(data[["cards"]][["fields"]][["funding_round_identifier"]])
      organization_identifier <- listParse(data[["cards"]][["fields"]][["organization_identifier"]])
      # Currency List parsing
      funding_round_money_raised <- currencyListParse(data[["cards"]][["fields"]][["funding_round_money_raised"]])
      
      # Put into one dateframe
      df <- data.frame(cbind(  uuid ,
                               permalink,
                               name,
                               announced_on ,
                               investor_type ,
                               funding_round_investment_type,
                               is_lead_investor ,
                               created_at ,
                               updated_at, # Simple parsing done
                               investor_identifier ,
                               funding_round_identifier ,
                               organization_identifier , # List parsing
                               funding_round_money_raised # Currency list parsing done
                               
      ))
      
      # Adjust classes
      # as numeric
      df$funding_round_money_raised <- as.numeric(df$funding_round_money_raised)
      # as date
      df$announced_on <- as.Date(df$announced_on)
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