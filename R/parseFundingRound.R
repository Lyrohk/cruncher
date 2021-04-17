#' Function to parse Funding Round Entity from JSON list to Data.frame
#'
#'@param data Returned fields card response in json list form for the entity 
#'@return  a data.frame of parsed output
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' parseFundingRound(data$cards$fields)
#'
#'
# Parse Funding Round Entity
parseFundingRound <- function(data) {
  
  # Identifier parse
  ids <- parseIdentifier(data[["identifier"]], "identifier")
  funded_organization_identifier <- parseIdentifier(data[["funded_organization_identifier"]], "funded_organization_identifier")
  investor_identifiers <- parseIdentifier(data[["investor_identifiers"]], "investor_identifiers")
  lead_investor_identifiers <- parseIdentifier(data[["lead_investor_identifiers"]], "lead_investor_identifiers")
  
  # Simple parse
  image_id <- simpleParse(data[["image_id"]])
  investment_type <- simpleParse(data[["investment_type"]])
  short_description <- simpleParse(data[["short_description"]])
  funded_organization_description <- simpleParse(data[["funded_organization_description"]])
  is_equity <- simpleParse(data[["is_equity"]]) #BOOLEAN!
  num_investors <- simpleParse(data[["num_investors"]])
  investment_stage <- simpleParse(data[["investment_stage"]])
  funded_organization_funding_stage <- simpleParse(data[["funded_organization_funding_stage"]])
  rank_funding_round <- simpleParse(data[["rank_funding_round"]])
  rank <- simpleParse(data[["rank"]])
  funded_organization_revenue_range <- simpleParse(data[["funded_organization_revenue_range"]])
  num_partners <- simpleParse(data[["num_partners"]])
  num_relationships <- simpleParse(data[["num_relationships"]])
  created_at <- simpleParse(data[["created_at"]])
  updated_at <- simpleParse(data[["updated_at"]])
  announced_on <- simpleParse(data[["announced_on"]])
  closed_on <- simpleParse(data[["closed_on"]])
  
  # List parsing
  funded_organization_location <- listParse(data[["funded_organization_location"]])
  funded_organization_categories <- listParse(data[["funded_organization_categories"]])
  
  # Currency list parsing
  funded_organization_funding_total <- currencyListParse(data[["funded_organization_funding_total"]])
  pre_money_valuation <- currencyListParse(data[["pre_money_valuation"]])
  post_money_valuation <- currencyListParse(data[["post_money_valuation"]])
  money_raised <- currencyListParse(data[["money_raised"]])
  target_money_raised <- currencyListParse(data[["target_money_raised"]])
  
  # Put into one dateframe
  df <- data.frame(cbind(ids,
                         announced_on ,
                         closed_on,
                         short_description ,
                         funded_organization_description ,
                         is_equity,
                         num_investors, 
                         num_relationships,
                         num_partners,
                         investment_type ,
                         investment_stage ,
                         rank,
                         rank_funding_round,
                         funded_organization_identifier ,
                         funded_organization_revenue_range,
                         funded_organization_funding_stage,
                         funded_organization_location, 
                         funded_organization_categories, 
                         funded_organization_funding_total ,
                         investor_identifiers ,
                         lead_investor_identifiers ,
                         pre_money_valuation ,
                         post_money_valuation ,
                         money_raised,
                         target_money_raised, 
                         created_at,
                         updated_at,
                         image_id
  ))
  
  # Adjust classes
  # as numeric
  df$rank <- as.numeric(df$rank)
  df$rank_funding_round <- as.numeric(df$rank_funding_round)
  df$num_partners <- as.numeric(df$num_partners)
  df$num_relationships <- as.numeric(df$num_relationships)
  df$num_investors <- as.numeric(df$num_investors)
  df$funded_organization_funding_total <- as.numeric(df$funded_organization_funding_total)
  df$pre_money_valuation <- as.numeric(df$pre_money_valuation)
  df$post_money_valuation <- as.numeric(df$post_money_valuation)
  df$money_raised <- as.numeric(df$money_raised)
  df$target_money_raised <- as.numeric(df$target_money_raised)
  
  # as Date
  df$announced_on <- as.Date(df$announced_on)
  df$closed_on <- as.Date(df$closed_on)
  df$created_at <- as.Date(df$created_at)
  df$updated_at <- as.Date(df$updated_at)
  
  # Return dataframe
  return(df)

}