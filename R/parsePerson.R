#' Function to parse Person Entity from JSON list to Data.frame
#'
#'@param data Returned fields card response in json list form for the entity 
#'@return  a data.frame of parsed output
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' parsePerson(data$cards$fields)
#'
#' @export
#'
# Parse Person Entity
parsePerson <- function(data) {
  
  # Identifier
  ids <- parseIdentifier(data[["identifier"]])
  
  # Simple parse character or numeric elements
  first_name <- simpleParse(data[["first_name"]])
  middle_name <- simpleParse(data[["middle_name"]])
  last_name <- simpleParse(data[["last_name"]])
  gender <- simpleParse(data[["gender"]])
  aliases <- simpleParse(data[["aliases"]])
  born_on <- simpleParse(data[["born_on"]]) #date
  died_on <- simpleParse(data[["died_on"]]) #date
  short_description <- simpleParse(data[["short_description"]])
  description <- simpleParse(data[["description"]])
  primary_job_title <- simpleParse(data[["primary_job_title"]])
  rank <- simpleParse(data[["rank"]])
  rank_person <- simpleParse(data[["rank_person"]])
  num_founded_organizations <- simpleParse(data[["num_founded_organizations"]])
  num_current_advisor_jobs <- simpleParse(data[["num_current_advisor_jobs"]])
  num_past_advisor_jobs <- simpleParse(data[["num_past_advisor_jobs"]])
  num_jobs <- simpleParse(data[["num_jobs"]])
  num_current_jobs <- simpleParse(data[["num_current_jobs"]])
  num_past_jobs <- simpleParse(data[["num_past_jobs"]])
  investor_type <- simpleParse( data[["investor_type"]])
  num_investments <- simpleParse(data[["num_investments"]])
  num_investments_funding_rounds <- simpleParse(data[["num_investments_funding_rounds"]])
  num_portfolio_organizations <- simpleParse(data[["num_portfolio_organizations"]])
  num_lead_investments <- simpleParse(data[["num_lead_investments"]])
  num_partner_investments <- simpleParse(data[["num_partner_investments"]])
  num_diversity_spotlight_investments <- simpleParse(data[["num_diversity_spotlight_investments"]])
  investor_stage <- simpleParse(data[["investor_stage"]])
  num_articles <- simpleParse(data[["num_articles"]])
  num_event_appearances <- simpleParse(data[["num_event_appearances"]])
  image_url <- simpleParse(data[["image_url"]])
  num_exits <- simpleParse(data[["num_exits"]])
  num_exits_ipo <- simpleParse(data[["num_exits_ipo"]])
  created_at <- simpleParse(data[["created_at"]])
  updated_at <- simpleParse( data[["updated_at"]])
  rank_delta_d90 <- simpleParse( data[["rank_delta_d90"]])
  rank_delta_d30 <- simpleParse(data[["rank_delta_d30"]])
  rank_delta_d7 <- simpleParse(data[["rank_delta_d7"]])
  rank_principal <- simpleParse(data[["rank_principal"]])
  rank_principal_investor <- simpleParse(data[["rank_principal_investor"]])
  num_relationships <- simpleParse(data[["num_relationships"]])
  
  # List parsing
  primary_organization <- listParse(data[["primary_organization"]])
  location_identifiers <- listParse(data[["location_identifiers"]])
  location_group_identifiers <- listParse(data[["location_group_identifiers"]])
  twitter <- listParse(data[["twitter"]])
  facebook <- listParse(data[["facebook"]])
  linkedin <- listParse(data[["linkedin"]])
  website <- listParse(data[["website"]])
  
  # Return as dataframe
  df <- data.frame(cbind(ids,
                         first_name, 
                         middle_name,
                         last_name,
                         gender,
                         born_on,
                         died_on,
                         short_description,
                         description,
                         aliases,
                         primary_job_title,
                         primary_organization,
                         rank,
                         rank_person,
                         num_relationships,
                         num_founded_organizations,
                         num_current_advisor_jobs,
                         num_past_advisor_jobs,
                         num_jobs,
                         num_current_jobs,
                         num_past_jobs,
                         investor_type,
                         num_investments,
                         num_investments_funding_rounds,
                         num_portfolio_organizations,
                         num_lead_investments,
                         num_partner_investments,
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
                         rank_delta_d7, 
                         rank_principal, 
                         rank_principal_investor, #Simple Parsing DON
                         location_identifiers,
                         location_group_identifiers,
                         twitter,
                         facebook,
                         linkedin,
                         website # List Parsing DONE
  ))
  
  # Convert numeric columns to numeric class from character
  df$rank <- as.numeric(df$rank)
  df$rank_delta_d90 <- as.numeric(df$rank_delta_d90)
  df$rank_delta_d30 <- as.numeric(df$rank_delta_d30)
  df$rank_delta_d7 <- as.numeric(df$rank_delta_d7)
  df$rank_principal <- as.numeric(df$rank_principal)
  df$rank_principal_investor <- as.numeric(df$rank_principal_investor)
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
  df$died_on <- as.Date(df$died_on)
  df$created_at <- as.Date(df$created_at)
  df$updated_at <- as.Date(df$updated_at)
  
  # Return dataframe
  return(df)
}