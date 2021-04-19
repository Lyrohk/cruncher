#' Function to parse Fund Entity from JSON list to Data.frame
#'
#'@param data Returned fields card response in json list form for the entity
#'@return  a data.frame of parsed output
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' parseEvent(data$cards$fields)
#'
#'@import stringr
#'
parseFund <- function(data) {
  # Identifier parsing
  ids <- parseIdentifier(data[["identifier"]], "identifier")
  investor_identifiers <- parseIdentifier(data[["investor_identifiers"]], "investor_identifiers")
  owner_identifier <- parseIdentifier(data[["owner_identifier"]], "owner_identifier")

  # Simple parsing
  short_description <- simpleParse(data[["short_description"]])
  image_id <- simpleParse(data[["image_id"]])
  num_investors <- simpleParse(data[["num_investors"]])
  rank <- simpleParse(data[["rank"]])
  started_on <- simpleParse(data[["started_on"]])
  announced_on <- simpleParse(data[["announced_on"]])
  created_at <- simpleParse(data[["created_at"]])
  updated_at <- simpleParse(data[["updated_at"]])

  # Currency parsing
  money_raised <- currencyListParse(data[["money_raised"]], "money_raised")

  # Put into one dateframe
  df <- data.frame(cbind(  ids ,
                           investor_identifiers,
                           owner_identifier,
                           short_description,
                           announced_on,
                           num_investors,
                           rank,
                           image_id,
                           started_on,
                           created_at ,
                           updated_at, # Simple parsing done
                           money_raised


  ))

  # Adjust classes
  # as numeric
  df$money_raised_value <- as.numeric(df$money_raised_value)
  df$money_raised_value_usd <- as.numeric(df$money_raised_value_usd)
  df$num_investors <- as.numeric(df$num_investors)
  df$rank <- as.numeric(df$rank)

  # as date
  df$started_on <- as.Date(df$started_on)
  df$announced_on <- as.Date(df$announced_on)
  df$created_at <- as.Date(df$created_at)
  df$updated_at <- as.Date(df$updated_at)

  # Return dataframe
  return(df)
}