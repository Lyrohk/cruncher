#' Function to lookup a single person over the Entity Lookup API Endpoint
#'
#'@param id UUID or permalink of the person you wish to look up
#'@param parse TRUE or FALSE. By default TRUE. If set to FALSE, it will return the data directly from the JSON, if set to TRUE, it will parse it into a data.frame object
#'@return either a data.frame (if parse = TRUE) or a list (if parse = FALSE)
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' lookUpPerson("facebook")
#'
#' @import httr
#' @import jsonlite
#' @export
#'
# Call the function to get you information about an organization
lookUpPerson <- function(id, please_parse = TRUE)  {
  
  # Check that API_KEY exists
  if (missing(API_KEY)) {
    stop("Please set your api key to the environment variable API_KEY or use setAPIKey() function to do so.")
  }
  
  # Check that id has been specified 
  if (missing(id)) {
    stop("Please enter a uuid or permalink for the person you wish to look up. You can find them manually from the browser or csv files.")
  }
  
  # Create the path
  url <- paste0("https://api.crunchbase.com/api/v4/entities/people/", id, "?card_ids=fields&user_key=", API_KEY)
  
  # Make http GET request
  response <- RETRY(verb = "GET", url = url) # Could use GET but someone may apply it with a list
  
  # Check if we get valid data, if not return error core
  if (response$status_code == 200) {
    data <- fromJSON(rawToChar(response$content))
    # Check if parsing is wanted
    if (please_parse) {
      
      # Parse organization to return a dataframe
      # Read out the properties
      uuid <- data[["properties"]][["identifier"]][["uuid"]]
      name <- data[["properties"]][["identifier"]][["value"]]
      
      # Simple parse character or numeric elements
      first_name <- simpleParse(data[["cards"]][["fields"]][["first_name"]])
      last_name <- simpleParse(data[["cards"]][["fields"]][["last_name"]])
      gender <- simpleParse(data[["cards"]][["fields"]][["gender"]])
      born_on <- simpleParse(data[["cards"]][["fields"]][["born_on"]])
      short_description <- simpleParse(data[["cards"]][["fields"]][["short_description"]])
      description <- simpleParse(data[["cards"]][["fields"]][["description"]])
      primary_job_title <- simpleParse(data[["cards"]][["fields"]][["primary_job_title"]])
      rank_person <- simpleParse(data[["cards"]][["fields"]][["rank_person"]])
      num_founded_organizations <- simpleParse(data[["cards"]][["fields"]][["num_founded_organizations"]])
      num_current_advisor_jobs <- simpleParse(data[["cards"]][["fields"]][["num_current_advisor_jobs"]])
      num_past_advisor_jobs <- simpleParse(data[["cards"]][["fields"]][["num_past_advisor_jobs"]])
      num_jobs <- simpleParse(data[["cards"]][["fields"]][["num_jobs"]])
      num_current_jobs <- simpleParse(data[["cards"]][["fields"]][["num_current_jobs"]])
      num_past_jobs <- simpleParse(data[["cards"]][["fields"]][["num_past_jobs"]])
      investor_type <- simpleParse( data[["cards"]][["fields"]][["investor_type"]])
      num_investments <- simpleParse(data[["cards"]][["fields"]][["num_investments"]])
      num_portfolio_organizations <- simpleParse(data[["cards"]][["fields"]][["num_portfolio_organizations"]])
      num_lead_investments <- simpleParse(data[["cards"]][["fields"]][["num_lead_investments"]])
      num_diversity_spotlight_investments <- simpleParse(data[["cards"]][["fields"]][["num_diversity_spotlight_investments"]])
      investor_stage <- simpleParse(data[["cards"]][["fields"]][["investor_stage"]])
      num_articles <- simpleParse(data[["cards"]][["fields"]][["num_articles"]])
      num_event_appearances <- simpleParse(data[["cards"]][["fields"]][["num_event_appearances"]])
      image_url <- simpleParse(data[["cards"]][["fields"]][["image_url"]])
      num_exits <- simpleParse(data[["cards"]][["fields"]][["num_exits"]])
      num_exits_ipo <- simpleParse(data[["cards"]][["fields"]][["num_exits_ipo"]])
      created_at <- simpleParse(data[["cards"]][["fields"]][["created_at"]])
      updated_at <- simpleParse( data[["cards"]][["fields"]][["updated_at"]])
      rank_delta_d90 <- simpleParse( data[["cards"]][["fields"]][["rank_delta_d90"]])
      rank_delta_d30 <- simpleParse(data[["cards"]][["fields"]][["rank_delta_d30"]])
      rank_delta_d7 <- simpleParse(data[["cards"]][["fields"]][["rank_delta_d7"]])
      
      # List parsing
      primary_organization <- listParse(data[["cards"]][["fields"]][["primary_organization"]])
      location_identifiers <- listParse(data[["cards"]][["fields"]][["location_identifiers"]])
      location_group_identifiers <- listParse(data[["cards"]][["fields"]][["location_group_identifiers"]])
      twitter <- listParse(data[["cards"]][["fields"]][["twitter"]])
      facebook <- listParse(data[["cards"]][["fields"]][["facebook"]])

      # Return as dataframe
      df <- data.frame(cbind(uuid, name,
                             first_name, 
                             last_name,
                             gender,
                             born_on,
                             short_description,
                             description,
                             primary_job_title,
                             rank_person,
                             num_founded_organizations,
                             num_current_advisor_jobs,
                             num_past_advisor_jobs,
                             num_jobs,
                             num_current_jobs,
                             num_past_jobs,
                             investor_type,
                             num_investments,
                             num_portfolio_organizations,
                             num_lead_investments,
                             num_diversity_spotlight_investments,
                             investor_stage,
                             num_articles,
                             num_event_appearances,
                             image_url,
                             num_exits,
                             num_exits_ipo,
                             created_at,
                             updated_at,
                             rank_delta_d90,
                             rank_delta_d30,
                             rank_delta_d7, #Simple Parsing DONE
                             primary_organization,
                             location_identifiers,
                             location_group_identifiers,
                             twitter,
                             facebook # List Parsing DONE
      ))
      
      # Convert numeric columns to numeric class from character
      df$rank_delta_d90 <- as.numeric(df$rank_delta_d90)
      df$rank_delta_d30 <- as.numeric(df$rank_delta_d30)
      df$rank_delta_d7 <- as.numeric(df$rank_delta_d7)
      df$num_exits_ipo <- as.numeric(df$num_exits_ipo)
      df$num_exits <- as.numeric(df$num_exits)
      df$num_event_appearances <- as.numeric(df$num_event_appearances)
      df$num_articles <- as.numeric(df$num_articles)
      df$num_diversity_spotlight_investments <- as.numeric(df$num_diversity_spotlight_investments)
      df$num_lead_investments <- as.numeric(df$num_lead_investments)
      df$num_portfolio_organizations <- as.numeric(df$num_portfolio_organizations)
      df$num_investments <- as.numeric(df$num_investments)
      df$rank_person <- as.numeric(df$rank_person)
      df$num_founded_organizations <- as.numeric(df$num_founded_organizations)
      df$num_current_advisor_jobs <- as.numeric(df$num_current_advisor_jobs)
      df$num_past_advisor_jobs <- as.numeric(df$num_past_advisor_jobs)
      df$num_jobs <- as.numeric(df$num_jobs)
      df$num_current_jobs <- as.numeric(df$num_current_jobs)
      df$num_past_jobs <- as.numeric(df$num_past_jobs)
      
      # Convert to simple Date
      df$born_on <- as.Date(df$born_on)
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