#' Function to parse Principal Entity from JSON list to Data.frame
#'
#'@param data Returned response in json list form for the entity
#'@return  a data.frame of parsed output
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' parsePrincipal(data$fields)
#'
#'@import stringr
#'
# Call the function to get you parse information about an entity
parsePrincipal <- function(data) {

  # Identifiers
  ids <- parseIdentifier(fields_data = data[["identifier"]], field = "identifier")
  founder_identifiers <- parseIdentifier(fields_data = data[["founder_identifiers"]], field = "founder_identifiers")
  investor_identifiers <- parseIdentifier(fields_data = data[["investor_identifiers"]], field = "investor_identifiers")
  acquirer_identifiers <- parseIdentifier(fields_data = data[["acquirer_identifiers"]], field = "acquirer_identifiers")
  owner_identifiers <- parseIdentifier(fields_data = data[["owner_identifiers"]], field = "owner_identifiers")


  company_type <- str_replace_all(simpleParse(data[["company_type"]]),
                                  pattern = c("for_profit" = "For Profit",
                                              "non_profit" = "Non-profit"))


  legal_name <- simpleParse(data[["legal_name"]])
  num_employees_enum <- str_replace_all(simpleParse(data[["num_employees_enum"]]),
                                        pattern = c(
                                          "c_00001_00010" = "1-10",
                                          "c_00011_00050" = "11-50",
                                          "c_00051_00100" = "51-100",
                                          "c_00101_00250" = "101-250",
                                          "c_00251_00500" = "251-500",
                                          "c_00501_01000" = "501-1000",
                                          "c_01001_05000" = "1001-5000",
                                          "c_05001_10000" = "5001-10000",
                                          "c_10001_max" = "10001+"
                                        ))
  operating_status <- simpleParse(data[["operating_status"]])
  status <- simpleParse(data[["status"]])
  revenue_range <-convertRevenueRange(simpleParse(data[["revenue_range"]]))
  stock_exchange_symbol <- simpleParse(data[["stock_exchange_symbol"]])
  short_description <- simpleParse(data[["short_description"]])
  went_public_on <- simpleParse(data[["went_public_on"]])
  description <- simpleParse(data[["description"]])
  ipo_status <- str_replace_all(simpleParse(data[["ipo_status"]]),
                                pattern = c(
                                  "delisted" = "Delisted",
                                  "private" = "Private",
                                  "public" = "Public"
                                ))
  image_id <- simpleParse(data[["image_id"]])
  image_url <- simpleParse(data[["image_url"]])
  last_equity_funding_type <- str_replace_all(simpleParse(data[["last_equity_funding_type"]]),
                                              pattern = c(
                                                "angel" = "Angel",
                                                "convertible_note" = "Convertible Note",
                                                "corporate_round" = "Corporate Round",
                                                "debt_financing" = "Debt Financing",
                                                "equity_crowdfunding" = "Equity Crowdfunding",
                                                "grant" = "Grant",
                                                "initial_coin_offering" = "Initial Coin Offering",
                                                "non_equity_assistance" = "Non-equity Assistance",
                                                "post_ipo_debt" = "Post-IPO Debt",
                                                "post_ipo_equity" = "Post-IPO Equity",
                                                "post_ipo_secondary" = "Post-IPO Secondary",
                                                "pre_seed" = "Pre-Seed",
                                                "private_equity" = "Private Equity",
                                                "product_crowdfunding" = "Product Crowdfunding",
                                                "secondary_market" = "Secondary Market",
                                                "seed" = "Seed",
                                                "series_a" = "Series A",
                                                "series_b" = "Series B",
                                                "series_c" = "Series C",
                                                "series_d" = "Series D",
                                                "series_e" = "Series E",
                                                "series_f" = "Series F",
                                                "series_g" = "Series G",
                                                "series_h" = "Series H",
                                                "series_i" = "Series I",
                                                "series_j" = "Series J",
                                                "series_unknown" = "Venture - Series Unknown",
                                                "undisclosed" = "Undisclosed"
                                              )
  )
  hub_tags <- simpleParse(data[["hub_tags"]])
  valuation_date <- simpleParse(data[["valuation_date"]])
  last_funding_type <- str_replace_all(simpleParse(data[["last_funding_type"]]),
                                       pattern = c(
                                         "angel" = "Angel",
                                         "convertible_note" = "Convertible Note",
                                         "corporate_round" = "Corporate Round",
                                         "debt_financing" = "Debt Financing",
                                         "equity_crowdfunding" = "Equity Crowdfunding",
                                         "grant" = "Grant",
                                         "initial_coin_offering" = "Initial Coin Offering",
                                         "non_equity_assistance" = "Non-equity Assistance",
                                         "post_ipo_debt" = "Post-IPO Debt",
                                         "post_ipo_equity" = "Post-IPO Equity",
                                         "post_ipo_secondary" = "Post-IPO Secondary",
                                         "pre_seed" = "Pre-Seed",
                                         "private_equity" = "Private Equity",
                                         "product_crowdfunding" = "Product Crowdfunding",
                                         "secondary_market" = "Secondary Market",
                                         "seed" = "Seed",
                                         "series_a" = "Series A",
                                         "series_b" = "Series B",
                                         "series_c" = "Series C",
                                         "series_d" = "Series D",
                                         "series_e" = "Series E",
                                         "series_f" = "Series F",
                                         "series_g" = "Series G",
                                         "series_h" = "Series H",
                                         "series_i" = "Series I",
                                         "series_j" = "Series J",
                                         "series_unknown" = "Venture - Series Unknown",
                                         "undisclosed" = "Undisclosed"
                                       ))
  created_at <- simpleParse(data[["created_at"]])
  last_funding_at <- simpleParse(data[["last_funding_at"]])
  listed_stock_symbol <- simpleParse(data[["listed_stock_symbol"]])
  website_url <- simpleParse(data[["website_url"]])
  updated_at <- simpleParse(data[["updated_at"]])
  funding_stage <-str_replace_all(simpleParse(data[["funding_stage"]]),
                                  pattern = c(
                                    "early_stage_venture" = "Early Stage Venture",
                                    "ipo" = "IPO",
                                    "late_stage_venture" = "Late Stage Venture",
                                    "m_and_a" = "M&A",
                                    "private_equity" = "Private Equity",
                                    "seed" = "Seed"
                                  ))

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
  investor_stage <- str_replace_all(simpleParse(data[["investor_stage"]]),
                                    pattern = c(
                                      "convertible_note" = "Convertible Note",
                                      "crowdfunding" = "Crowdfunding",
                                      "debt" = "Debt",
                                      "early_stage_venture" = "Early Stage Venture",
                                      "grant" = "Grant",
                                      "late_stage_venture" = "Late Stage Venture",
                                      "initial_coin_offering" = "Initial Coin Offering",
                                      "non_equity_assistance" = "Non-equity Assistance",
                                      "post_ipo" = "Post-Ipo",
                                      "private_equity" = "Private Equity",
                                      "secondary_market" = "Secondary Market",
                                      "seed" = "Seed",
                                      "venture " = "Venture"
                                    ))
  investor_type <- str_replace_all(simpleParse(data[["investor_type"]]),
                                   pattern = c(
                                     "accelerator" - "Accelerator",
                                     "angel" - "Individual/Angel",
                                     "angel_group" - "Angel Group",
                                     "co_working_space" - "Co-Working Space",
                                     "corporate_venture_capital" - "Corporate Venture Capital",
                                     "entrepreneurship_program" - "Entrepreneurship Program",
                                     "family_investment_office" - "Family Investment Office",
                                     "fund_of_funds" - "Fund Of Funds",
                                     "government_office" - "Government Office",
                                     "hedge_fund" - "Hedge Fund",
                                     "incubator" - "Incubator",
                                     "investment_bank" - "Investment Bank",
                                     "investment_partner" - "Investment Partner",
                                     "micro_vc" - "Micro VC",
                                     "pension_funds" - "Pension Funds",
                                     "private_equity_firm" - "Private Equity Firm",
                                     "secondary_purchaser" - "Secondary Purchaser",
                                     "startup_competition" - "Startup Competition",
                                     "syndicate" - "Syndicate",
                                     "university_program" - "University Program",
                                     "venture_capital" - "Venture Capital",
                                     "venture_debt" - "Venture Debt"
                                   ))
  aliases <- simpleParse(data[["aliases"]])
  born_on <- simpleParse(data[["born_on"]])
  died_on <- simpleParse(data[["died_on"]])


  # Read out simple lists vs. S3 dataframe lists
  founded_on <- listParse(data[["founded_on"]])
  # Other
  contact_email <- simpleParse(data[["contact_email"]])
  layout_id <- simpleParse(data[["layout_id"]])
  num_alumni <- simpleParse(data[["num_alumni"]])
  num_enrollments <-str_replace_all(simpleParse(data[["num_enrollments"]]),
                                    pattern = c(
                                      "c_00001_00010" = "1-10",
                                      "c_00011_00050" = "11-50",
                                      "c_00051_00100" = "51-100",
                                      "c_00101_00250" = "101-250",
                                      "c_00251_00500" = "251-500",
                                      "c_00501_01000" = "501-1000",
                                      "c_01001_05000" = "1001-5000",
                                      "c_05001_10000" = "5001-10000",
                                      "c_10001_max" = "10001+"
                                    ))
  num_jobs <- simpleParse(data[["num_jobs"]])
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
  rank_person <- simpleParse(data[["rank_person"]])
  rank_principal_investor <- simpleParse(data[["rank_principal_investor"]])
  school_method <-str_replace_all(simpleParse(data[["school_method"]]),
                                  pattern = c(
                                    "on_compus" = "On Campus",
                                    "online" = "Online",
                                    "online_and_on_campus" = "Online and On Campus"
                                  ))
  school_program <- str_replace_all(simpleParse(data[["school_program"]]),
                                    pattern = c(
                                      "bootcamp" = "Bootcamp",
                                      "community_college" = "Community College",
                                      "four_year_university" = "Four Year University",
                                      "graduate_university" = "Graduate University",
                                      "high_school" = "High School",
                                      "trade_school" = "Trade School",
                                      "two_year_university" = "Two Year University"
                                    ))
  school_type <-str_replace_all(simpleParse(data[["school_type"]]),
                                pattern = c(
                                  "for_profit_private" - "Private",
                                  "non_profit_private" - "Private (Non-Profit)",
                                  "public" - "Public"
                                ))
  facet_ids <- simpleParse(data[["facet_ids"]])
  first_name <- simpleParse(data[["first_name"]])
  last_name <- simpleParse(data[["last_name"]])
  gender <- simpleParse(data[["gender"]])
  num_founded_organizations <- simpleParse(data[["num_founded_organizations"]])
  num_partner_investments <- simpleParse(data[["num_partner_investments"]])
  primary_job_title <- simpleParse(data[["primary_job_title"]])


  # List parsing
  closed_on <- listParse(data[["closed_on"]])
  delisted_on <- listParse(data[["delisted_on"]])
  demo_days <- listParse(data[["demo_days"]])
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
  primary_organization <- listParse(data[["primary_organization"]])

  # Currenty parsing lists to USD
  last_funding_total <- currencyListParse(fields_data = data[["last_funding_total"]], field = "last_funding_total")
  last_equity_funding_total <- currencyListParse(fields_data = data[["last_equity_funding_total"]], field = "last_equity_funding_total")
  funding_total <- currencyListParse(fields_data = data[["funding_total"]], field = "funding_total")
  valuation <- currencyListParse(fields_data = data[["valuation"]], field = "valuation")
  equity_funding_total <- currencyListParse(fields_data = data[["equity_funding_total"]], field = "equity_funding_total")
  funds_total <- currencyListParse(fields_data = data[["funds_total"]], field = "funds_total")


  # Return these in one dataframe
  return(data.frame(cbind(ids,
                          first_name,
                          last_name,
                          gender,
                          primary_job_title,
                          primary_organization,
                          legal_name, company_type, status, operating_status,
                          short_description, description, num_employees_enum, revenue_range,
                          went_public_on, ipo_status,
                          image_url,
                          image_id,
                          last_funding_at, last_equity_funding_type,
                          last_funding_type, funding_stage, hub_tags, valuation_date, stock_exchange_symbol,
                          listed_stock_symbol, website_url, updated_at, created_at, # Characters done
                          rank_org,
                          rank_person,
                          num_founders, num_lead_investors, num_investors, num_funding_rounds,
                          num_articles,num_event_appearances,num_current_positions,num_current_advisor_positions,
                          num_diversity_spotlight_investments,
                          num_jobs,
                          num_partner_investments,
                          num_acquisitions, num_sub_organizations, num_investments,
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
                          num_founded_organizations,
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
                          born_on,
                          died_on,
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