#' Function to lookup a single funding round over the Entity Lookup API Endpoint
#'
#'@param id UUID or permalink of the funding round you wish to look up
#'@param parse TRUE or FALSE. By default TRUE. If set to FALSE, it will return the data directly from the JSON, if set to TRUE, it will parse it into a data.frame object
#'@return either a data.frame (if parse = TRUE) or a list (if parse = FALSE)
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' lookUpFundingRound("skydio")
#'
#' @import httr
#' @import jsonlite
#' @export
#'
# Call the function to get you information about an organization
lookUpFundingRound <- function(id, please_parse = TRUE)  {
  
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
  url <- paste0("https://api.crunchbase.com/api/v4/entities/funding_rounds/", id, "?card_ids=fields&user_key=", API_KEY)
  
  # Make http GET request
  response <- RETRY(verb = "GET", url = url) # Could use GET but someone may apply it with a list
  
  # Check if we get valid data, if not return error core
  if (response$status_code == 200) {
    data <- fromJSON(rawToChar(response$content))
    # Check if parsing is wanted
    if (please_parse) {
      
      # Parse funding round
      # Simple parsing
      uuid <- simpleParse(data[["properties"]][["identifier"]][["uuid"]])
      name <- simpleParse(data[["properties"]][["identifier"]][["value"]])
      investment_type <- simpleParse(data[["cards"]][["fields"]][["investment_type"]])
      announced_on <- simpleParse(data[["cards"]][["fields"]][["announced_on"]])
      short_description <- simpleParse(data[["cards"]][["fields"]][["short_description"]])
      funded_organization_description <- simpleParse(data[["cards"]][["fields"]][["funded_organization_description"]])
      is_equity <- simpleParse(data[["cards"]][["fields"]][["is_equity"]]) #BOOLEAN!
      num_investors <- simpleParse(data[["cards"]][["fields"]][["num_investors"]])
      investment_stage <- simpleParse(data[["cards"]][["fields"]][["investment_stage"]])
      funded_organization_funding_stage <- simpleParse(data[["cards"]][["fields"]][["funded_organization_funding_stage"]])
      rank_funding_round <- simpleParse(data[["cards"]][["fields"]][["rank_funding_round"]])
      funded_organization_revenue_range <- simpleParse(data[["cards"]][["fields"]][["funded_organization_revenue_range"]])
      num_partners <- simpleParse(data[["cards"]][["fields"]][["num_partners"]])
      created_at <- simpleParse(data[["cards"]][["fields"]][["created_at"]])
      updated_at <- simpleParse(data[["cards"]][["fields"]][["updated_at"]])
      
      # List parsing
      funded_organization_identifier <- listParse(data[["cards"]][["fields"]][["funded_organization_identifier"]])
      investor_identifiers <- listParse(data[["cards"]][["fields"]][["investor_identifiers"]])
      lead_investor_identifiers <- listParse(data[["cards"]][["fields"]][["lead_investor_identifiers"]])
      funded_organization_location <- listParse(data[["cards"]][["fields"]][["funded_organization_location"]])
      funded_organization_categories <- listParse(data[["cards"]][["fields"]][["funded_organization_categories"]])
      # Currency list parsing
      funded_organization_funding_total <- currencyListParse(data[["cards"]][["fields"]][["funded_organization_funding_total"]])
      pre_money_valuation <- currencyListParse(data[["cards"]][["fields"]][["pre_money_valuation"]])
      post_money_valuation <- currencyListParse(data[["cards"]][["fields"]][["post_money_valuation"]])
      money_raised <- currencyListParse(data[["cards"]][["fields"]][["money_raised"]])
      
      # Put into one dateframe
      df <- data.frame(cbind(uuid,
                             name, 
                             investment_type ,
                             announced_on ,
                             short_description ,
                             funded_organization_description ,
                             is_equity,
                             num_investors, 
                             investment_stage ,
                             funded_organization_funding_stage,
                             rank_funding_round,
                             funded_organization_revenue_range,
                             num_partners,
                             created_at,
                             updated_at, # Simple parsing done
                             funded_organization_identifier ,
                             investor_identifiers ,
                             lead_investor_identifiers ,
                             funded_organization_location, 
                             funded_organization_categories, 
                             # Currency list parsing
                             funded_organization_funding_total ,
                             pre_money_valuation ,
                             post_money_valuation ,
                             money_raised # List parsing done
      ))
      
      # Adjust classes
      # as numeric
      df$num_partners <- as.numeric(df$num_partners)
      df$num_investors <- as.numeric(df$num_investors)
      df$funded_organization_funding_total <- as.numeric(df$funded_organization_funding_total)
      df$pre_money_valuation <- as.numeric(df$pre_money_valuation)
      df$post_money_valuation <- as.numeric(df$post_money_valuation)
      df$money_raised <- as.numeric(df$money_raised)
      
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
    printError(response$status_code)
  }
}