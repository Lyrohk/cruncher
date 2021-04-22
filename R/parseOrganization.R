#' Function to parse Organization Entity from JSON list to Data.frame
#'
#'@param data Returned response in json list form for the entity
#'@return  a data.frame of parsed output
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' lookUpOrganization("facebook")
#'
#'@import stringr
#' @export
#'
# Call the function to get you parse information about an entity
parseOrganization <- function(data) {

  # Identifiers
  ids <- parseIdentifier(fields_data = data[["identifier"]], field = "identifier")
  founder_identifiers <- parseIdentifier(fields_data = data[["founder_identifiers"]], field = "founder_identifiers")
  investor_identifiers <- parseIdentifier(fields_data = data[["investor_identifiers"]], field = "investor_identifiers")
  acquirer_identifiers <- parseIdentifier(fields_data = data[["acquirer_identifiers"]], field = "acquirer_identifiers")
  owner_identifiers <- parseIdentifier(fields_data = data[["owner_identifiers"]], field = "owner_identifiers")


  company_type <- str_replace_all(simpleParse(data[["company_type"]]),
                                  pattern = getCompanyTypes())


  legal_name <- simpleParse(data[["legal_name"]])
  num_employees_enum <- str_replace_all(simpleParse(data[["num_employees_enum"]]),
                                        pattern = getEmployeeRanges())
  operating_status <- simpleParse(data[["operating_status"]])
  status <- simpleParse(data[["status"]])
  revenue_range <-convertRevenueRange(simpleParse(data[["revenue_range"]]))
  stock_exchange_symbol <- simpleParse(data[["stock_exchange_symbol"]])
  short_description <- simpleParse(data[["short_description"]])
  went_public_on <- simpleParse(data[["went_public_on"]])
  description <- simpleParse(data[["description"]])
  ipo_status <- str_replace_all(simpleParse(data[["ipo_status"]]),
                                pattern = getIpoStatus())
  image_url <- simpleParse(data[["image_url"]])
  last_equity_funding_type <- str_replace_all(simpleParse(data[["last_equity_funding_type"]]),
                                              pattern = getFundingTypes())
  hub_tags <- simpleParse(data[["hub_tags"]])
  valuation_date <- simpleParse(data[["valuation_date"]])
  last_funding_type <- str_replace_all(simpleParse(data[["last_funding_type"]]),
                                       pattern = getFundingTypes())
  created_at <- simpleParse(data[["created_at"]])
  last_funding_at <- simpleParse(data[["last_funding_at"]])
  listed_stock_symbol <- simpleParse(data[["listed_stock_symbol"]])
  website_url <- simpleParse(data[["website_url"]])
  updated_at <- simpleParse(data[["updated_at"]])
  funding_stage <-str_replace_all(simpleParse(data[["funding_stage"]]),
                                  pattern = getFundingStages())

  # Numeric elements
  rank_org <- simpleParse(data[["rank_org"]])
  num_founders <- simpleParse(data[["num_founders"]])
  num_lead_investors <- simpleParse(data[["num_lead_investors"]])
  num_investors <- simpleParse(data[["num_investors"]])
  num_funding_rounds <- simpleParse(data[["num_funding_rounds"]])
  num_articles <- simpleParse(data[["num_articles"]])
  num_event_appearances <- simpleParse(data[["num_event_appearances"]])
  num_current_positions <- simpleParse(data[["num_current_positions"]])
  num_current_advisor_positions <- simpleParse(data[["num_current_advisor_positions"]])
  num_diversity_spotlight_investments <- simpleParse(data[["num_diversity_spotlight_investments"]])
  num_acquisitions <- simpleParse(data[["num_acquisitions"]])
  num_sub_organizations <- simpleParse(data[["num_sub_organizations"]])
  num_investments <- simpleParse(data[["num_investments"]])
  num_lead_investments <- simpleParse(data[["num_lead_investments"]])
  num_portfolio_organizations <- simpleParse(data[["num_portfolio_organizations"]])
  rank_delta_d7 <- simpleParse(data[["rank_delta_d7"]])
  rank_delta_d30 <- simpleParse(data[["rank_delta_d30"]])
  rank_delta_d90 <- simpleParse(data[["rank_delta_d90"]])
  investor_stage <- simpleParse(data[["investor_stage"]])
  investor_type <- simpleParse(data[["investor_type"]])
  aliases <- simpleParse(data[["aliases"]])


  # Read out simple lists vs. S3 dataframe lists
  founded_on <- listParse(data[["founded_on"]])
  # Other
  contact_email <- simpleParse(data[["contact_email"]])
  layout_id <- simpleParse(data[["layout_id"]])
  num_alumni <- simpleParse(data[["num_alumni"]])
  num_enrollments <-str_replace_all(simpleParse(data[["num_enrollments"]]),
                                    pattern = getEnrollmentsRanges())
  num_exits <- simpleParse(data[["num_exits"]])
  num_exits_ipo <- simpleParse(data[["num_exits_ipo"]])
  num_founder_alumni <- simpleParse(data[["num_founder_alumni"]])
  num_funds <- simpleParse(data[["num_funds"]])
  num_investments_funding_rounds <- simpleParse(data[["num_investments_funding_rounds"]])
  num_past_positions <- simpleParse(data[["num_past_positions"]])
  num_relationships <- simpleParse(data[["num_relationships"]])
  phone_number <- simpleParse(data[["phone_number"]])
  program_application_deadline <- simpleParse(data[["program_application_deadline"]])
  program_duration <- simpleParse(data[["program_duration"]])
  program_type <- simpleParse(data[["program_type"]])
  rank <- simpleParse(data[["rank"]])
  rank_org_company <- simpleParse(data[["rank_org_company"]])
  rank_org_school <- simpleParse(data[["rank_org_school"]])
  rank_principal <- simpleParse(data[["rank_principal"]])
  rank_principal_investor <- simpleParse(data[["rank_principal_investor"]])
  school_method <-str_replace_all(simpleParse(data[["school_method"]]),
                                  pattern = getSchoolMethods())
  school_program <- str_replace_all(simpleParse(data[["school_program"]]),
                                    pattern = getSchoolPrograms())
  school_type <-str_replace_all(simpleParse(data[["school_type"]]),
                                pattern = getSchoolTypes())
  closed_on <- listParse(data[["closed_on"]])
  delisted_on <- listParse(data[["delisted_on"]])
  demo_days <- listParse(data[["demo_days"]])
  facet_ids <- listParse(data[["facet_ids"]])
  location_identifiers <- listParse(data[["location_identifiers"]])
  location_group_identifiers <- listParse(data[["location_group_identifiers"]])
  categories <- listParse(data[["categories"]])
  category_groups <- listParse(data[["category_groups"]])
  website <- listParse(data[["website"]])
  twitter <- listParse(data[["twitter"]])
  facebook <- listParse(data[["facebook"]])
  linkedin <- listParse(data[["linkedin"]])
  exited_on <- listParse(data[["exited_on"]])
  stock_symbol <- listParse(data[["stock_symbol"]])

  # Currenty parsing lists to USD
  last_funding_total <- currencyListParse(fields_data = data[["last_funding_total"]], field = "last_funding_total")
  last_equity_funding_total <- currencyListParse(fields_data = data[["last_equity_funding_total"]], field = "last_equity_funding_total")
  funding_total <- currencyListParse(fields_data = data[["funding_total"]], field = "funding_total")
  valuation <- currencyListParse(fields_data = data[["valuation"]], field = "valuation")
  equity_funding_total <- currencyListParse(fields_data = data[["equity_funding_total"]], field = "equity_funding_total")
  funds_total <- currencyListParse(fields_data = data[["funds_total"]], field = "funds_total")


  # Return these in one dataframe
  return(data.frame(cbind(ids,
                           legal_name, company_type, status, operating_status,
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
}