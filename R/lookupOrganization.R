#' Function to lookup a single organization over the Entity Lookup API Endpoint
#'
#'@param id UUID or permalink of the organization you wish to look up
#'@param parse TRUE or FALSE. By default TRUE. If set to FALSE, it will return the data directly from the JSON, if set to TRUE, it will parse it into a data.frame object
#'@return either a data.frame (if parse = TRUE) or a list (if parse = FALSE)
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' lookUpOrganization("facebook")
#'
#' @import httr
#' @import jsonlite
#' @import stringr
#' @import dplyr
#' @export
#'
# Call the function to get you information about an organization
lookUpOrganization <- function(id, please_parse = TRUE)  {
  # Create the path
  url <- paste0("https://api.crunchbase.com/api/v4/entities/organizations/", id, "?card_ids=fields&user_key=", API_KEY)
  
  # Make http GET request
  response <- RETRY(verb = "GET", url = url) # Could use GET but someone may apply it with a list
  
  # Check if we get valid data, if not return error core
  if (response$status_code == 200) {
    data <- fromJSON(rawToChar(response$content))
    # Check if parsing is wanted
    if (please_parse) {
      
      # Parse organization to return a dataframe
      name <- listParse(data[["cards"]][["fields"]][["identifier"]])
      uuid <- simpleParse(data[["cards"]][["fields"]][["identifier"]][["uuid"]])
      company_type <- simpleParse(data[["cards"]][["fields"]][["company_type"]])
      legal_name <- simpleParse(data[["cards"]][["fields"]][["legal_name"]])
      num_employees_enum <- simpleParse(data[["cards"]][["fields"]][["num_employees_enum"]])
      operating_status <- simpleParse(data[["cards"]][["fields"]][["operating_status"]])
      status <- simpleParse(data[["cards"]][["fields"]][["status"]])
      revenue_range <- simpleParse(data[["cards"]][["fields"]][["revenue_range"]])
      stock_exchange_symbol <- simpleParse(data[["cards"]][["fields"]][["stock_exchange_symbol"]])
      short_description <- simpleParse(data[["cards"]][["fields"]][["short_description"]])
      went_public_on <- simpleParse(data[["cards"]][["fields"]][["went_public_on"]])
      description <- simpleParse(data[["cards"]][["fields"]][["description"]])
      ipo_status <- simpleParse(data[["cards"]][["fields"]][["ipo_status"]])
      image_url <- simpleParse(data[["cards"]][["fields"]][["image_url"]])
      last_equity_funding_type <- simpleParse(data[["cards"]][["fields"]][["last_equity_funding_type"]])
      hub_tags <- simpleParse(data[["cards"]][["fields"]][["hub_tags"]])
      valuation_date <- simpleParse(data[["cards"]][["fields"]][["valuation_date"]])
      last_funding_type <- simpleParse(data[["cards"]][["fields"]][["last_funding_type"]])
      created_at <- simpleParse(data[["cards"]][["fields"]][["created_at"]])
      last_funding_at <- simpleParse(data[["cards"]][["fields"]][["last_funding_at"]])
      listed_stock_symbol <- simpleParse(data[["cards"]][["fields"]][["listed_stock_symbol"]])
      website_url <- simpleParse(data[["cards"]][["fields"]][["website_url"]])
      updated_at <- simpleParse(data[["cards"]][["fields"]][["updated_at"]])
      funding_stage <- simpleParse(data[["cards"]][["fields"]][["funding_stage"]])
      # Numeric elements
      rank_org <- simpleParse(data[["cards"]][["fields"]][["rank_org"]])
      num_founders <- simpleParse(data[["cards"]][["fields"]][["num_founders"]])
      num_lead_investors <- simpleParse(data[["cards"]][["fields"]][["num_lead_investors"]])
      num_investors <- simpleParse(data[["cards"]][["fields"]][["num_investors"]])
      num_funding_rounds <- simpleParse(data[["cards"]][["fields"]][["num_funding_rounds"]])
      num_articles <- simpleParse(data[["cards"]][["fields"]][["num_articles"]])
      num_event_appearances <- simpleParse(data[["cards"]][["fields"]][["num_event_appearances"]])
      num_current_positions <- simpleParse(data[["cards"]][["fields"]][["num_current_positions"]])
      num_current_advisor_positions <- simpleParse(data[["cards"]][["fields"]][["num_current_advisor_positions"]])
      num_diversity_spotlight_investments <- simpleParse(data[["cards"]][["fields"]][["num_diversity_spotlight_investments"]])
      num_acquisitions <- simpleParse(data[["cards"]][["fields"]][["num_acquisitions"]])
      num_sub_organizations <- simpleParse(data[["cards"]][["fields"]][["num_sub_organizations"]])
      num_investments <- simpleParse(data[["cards"]][["fields"]][["num_investments"]])
      num_lead_investments <- simpleParse(data[["cards"]][["fields"]][["num_lead_investments"]])
      num_portfolio_organizations <- simpleParse(data[["cards"]][["fields"]][["num_portfolio_organizations"]])
      rank_delta_d7 <- simpleParse(data[["cards"]][["fields"]][["rank_delta_d7"]])
      rank_delta_d30 <- simpleParse(data[["cards"]][["fields"]][["rank_delta_d30"]])
      rank_delta_d90 <- simpleParse(data[["cards"]][["fields"]][["rank_delta_d90"]])
      
      # Read out simple lists vs. S3 dataframe lists
      founder_identifiers <- listParse(data[["cards"]][["fields"]][["founder_identifiers"]])
      founded_on <- listParse(data[["cards"]][["fields"]][["founded_on"]])
      # Other
      contact_email <- simpleParse(data[["cards"]][["fields"]][["contact_email"]])
      layout_id <- simpleParse(data[["cards"]][["fields"]][["layout_id"]])
      num_alumni <- simpleParse(data[["cards"]][["fields"]][["num_alumni"]])
      num_enrollments <- simpleParse(data[["cards"]][["fields"]][["num_enrollments"]])
      num_exits <- simpleParse(data[["cards"]][["fields"]][["num_exits"]])
      num_exits_ipo <- simpleParse(data[["cards"]][["fields"]][["num_exits_ipo"]])
      num_founder_alumni <- simpleParse(data[["cards"]][["fields"]][["num_founder_alumni"]])
      num_funds <- simpleParse(data[["cards"]][["fields"]][["num_funds"]])
      num_investments_funding_rounds <- simpleParse(data[["cards"]][["fields"]][["num_investments_funding_rounds"]])
      num_past_positions <- simpleParse(data[["cards"]][["fields"]][["num_past_positions"]])
      num_relationships <- simpleParse(data[["cards"]][["fields"]][["num_relationships"]])
      phone_number <- simpleParse(data[["cards"]][["fields"]][["phone_number"]])
      program_application_deadline <- simpleParse(data[["cards"]][["fields"]][["program_application_deadline"]])
      program_duration <- simpleParse(data[["cards"]][["fields"]][["program_duration"]])
      program_type <- simpleParse(data[["cards"]][["fields"]][["program_type"]])
      rank <- simpleParse(data[["cards"]][["fields"]][["rank"]])
      rank_org_company <- simpleParse(data[["cards"]][["fields"]][["rank_org_company"]])
      rank_org_school <- simpleParse(data[["cards"]][["fields"]][["rank_org_school"]])
      rank_principal <- simpleParse(data[["cards"]][["fields"]][["rank_principal"]])
      rank_principal_investor <- simpleParse(data[["cards"]][["fields"]][["rank_principal_investor"]])
      school_method <- simpleParse(data[["cards"]][["fields"]][["school_method"]])
      school_program <- simpleParse(data[["cards"]][["fields"]][["school_program"]])
      school_type <- simpleParse(data[["cards"]][["fields"]][["school_type"]])
      aliases <- listParse(data[["cards"]][["fields"]][["aliases"]])
      closed_on <- listParse(data[["cards"]][["fields"]][["closed_on"]])
      delisted_on <- listParse(data[["cards"]][["fields"]][["delisted_on"]])
      demo_days <- listParse(data[["cards"]][["fields"]][["demo_days"]])
      facet_ids <- listParse(data[["cards"]][["fields"]][["facet_ids"]])
      funds_total <- listParse(data[["cards"]][["fields"]][["funds_total"]])
      investor_stage <- listParse(data[["cards"]][["fields"]][["investor_stage"]])
      investor_type <- listParse(data[["cards"]][["fields"]][["investor_type"]])
      acquirer_identifiers <- listParse(data[["cards"]][["fields"]][["acquirer_identifiers"]])
      owner_identifiers <- listParse(data[["cards"]][["fields"]][["owner_identifiers"]])
      location_identifiers <- listParse(data[["cards"]][["fields"]][["location_identifiers"]])
      location_group_identifiers <- listParse(data[["cards"]][["fields"]][["location_group_identifiers"]])
      categories <- listParse(data[["cards"]][["fields"]][["categories"]])
      category_groups <- listParse(data[["cards"]][["fields"]][["category_groups"]])
      website <- listParse(data[["cards"]][["fields"]][["website"]])
      twitter <- listParse(data[["cards"]][["fields"]][["twitter"]])
      facebook <- listParse(data[["cards"]][["fields"]][["facebook"]])
      linkedin <- listParse(data[["cards"]][["fields"]][["linkedin"]])
      investor_identifiers <- listParse(data[["cards"]][["fields"]][["investor_identifiers"]])
      exited_on <- listParse(data[["cards"]][["fields"]][["exited_on"]])
      stock_symbol <- listParse(data[["cards"]][["fields"]][["stock_symbol"]])

      # Currenty parsing lists to USD
      last_funding_total <- currencyListParse(data[["cards"]][["fields"]][["last_funding_total"]])
      last_equity_funding_total <- currencyListParse(data[["cards"]][["fields"]][["last_equity_funding_total"]])
      funding_total <- currencyListParse(data[["cards"]][["fields"]][["funding_total"]])
      valuation <- currencyListParse(data[["cards"]][["fields"]][["valuation"]])
      equity_funding_total <- currencyListParse(data[["cards"]][["fields"]][["equity_funding_total"]])
      
      # Return these in one dataframe (i.e. one row with all 62 columns)
      return(data.frame(cbind(name, legal_name, uuid, company_type, status, operating_status,
                       short_description, description, num_employees_enum, revenue_range,
                       went_public_on, ipo_status, image_url, last_funding_at, last_equity_funding_type,
                       last_funding_type, funding_stage, hub_tags, valuation_date, stock_exchange_symbol, 
                       listed_stock_symbol, website_url, updated_at, created_at, # Characters done
                       rank_org, num_founders, num_lead_investors, num_investors, num_funding_rounds,
                       num_articles,num_event_appearances,num_current_positions,num_current_advisor_positions,
                       num_diversity_spotlight_investments, num_acquisitions, num_sub_organizations, num_investments,
                       num_lead_investments, num_portfolio_organizations, rank_delta_d7, rank_delta_d30, rank_delta_d90, # Numeric done
                       last_funding_total, last_equity_funding_total, funding_total, valuation, equity_funding_total, # Currency list done
                       founded_on, founder_identifiers ,  location_identifiers, location_group_identifiers ,
                       categories , category_groups , website ,
                       twitter ,facebook,  linkedin ,
                       investor_identifiers,exited_on ,stock_symbol ,
                       contact_email ,
                       layout_id ,
                       num_alumni ,
                       num_enrollments ,
                       num_exits ,
                       num_exits_ipo ,
                       num_founder_alumni ,
                       num_funds ,
                       num_investments_funding_rounds ,
                       num_past_positions ,
                       num_relationships ,
                       phone_number ,
                       program_application_deadline ,
                       program_duration ,
                       program_type ,
                       rank ,
                       rank_org_company ,
                       rank_org_school ,
                       rank_principal ,
                       rank_principal_investor ,
                       school_method ,
                       school_program ,
                       school_type ,
                       aliases ,
                       closed_on ,
                       delisted_on ,
                       demo_days ,
                       facet_ids ,
                       funds_total ,
                       investor_stage ,
                       investor_type ,
                       acquirer_identifiers ,
                       owner_identifiers 
      )))
      
    } else {
      # Return data
      return(data)
    }
  } else {
    # Print error code
    print(response$status_code)
  }
}