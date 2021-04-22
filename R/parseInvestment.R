#' Function to parse Investment Entity from JSON list to Data.frame
#'
#'@param data Returned fields card response in json list form for the entity
#'@return  a data.frame of parsed output
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' parseInvestment(data$cards$fields)
#'
#'@import stringr
#'
# Parse Investment Entity
parseInvestment <- function(data) {

  # Identifier
  ids <- parseIdentifier(data[["identifier"]], "identifier")
  investor_identifier <- parseIdentifier(data[["investor_identifier"]], "investor_identifier")
  funding_round_identifier <- parseIdentifier(data[["funding_round_identifier"]], "funding_round_identifier")
  organization_identifier <- parseIdentifier(data[["organization_identifier"]], "organization_identifier")
  partner_identifiers <- parseIdentifier(data[["partner_identifiers"]], "partner_identifiers")


  # Simple parse
  announced_on <- simpleParse(data[["announced_on"]])
  investor_stage <- str_replace_all(simpleParse(data[["investor_stage"]]),
                                    pattern = getInvestorStages())
  investor_type <- str_replace_all(simpleParse(data[["investor_type"]]),
                                   pattern = getInvestorTypes())
  is_lead_investor <- simpleParse(data[["is_lead_investor"]])
  created_at <- simpleParse(data[["created_at"]])
  updated_at <- simpleParse(data[["updated_at"]])

  # Currency List parsing
  money_invested <- currencyListParse(data[["money_invested"]], "money_invested")
  funding_round_money_raised <- currencyListParse(data[["funding_round_money_raised"]], "funding_round_money_raised")

  # Put into one dateframe
  df <- data.frame(cbind(  ids ,
                           investor_identifier ,
                           investor_stage,
                           investor_type ,
                           is_lead_investor,
                           money_invested,
                           funding_round_identifier ,
                           organization_identifier ,
                           partner_identifiers, # Identifier parsing done
                           announced_on ,
                           created_at ,
                           updated_at, # Simple parsing done
                           funding_round_money_raised # Currency list parsing done

  ))

  # Adjust classes

  # as numeric
  df$funding_round_money_raised_value <- as.numeric(df$funding_round_money_raised_value)
  df$funding_round_money_raised_value_usd <- as.numeric(df$funding_round_money_raised_value_usd)
  df$money_invested_value <- as.numeric(df$money_invested_value)
  df$money_invested_value_usd <- as.numeric(df$money_invested_value_usd)

  # as date
  df$announced_on <- as.Date(df$announced_on)
  df$created_at <- as.Date(df$created_at)
  df$updated_at <- as.Date(df$updated_at)

  # as logical
  df$is_lead_investor <- as.logical(is_lead_investor)

  # Return dataframe
  df
}