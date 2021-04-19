#' Function to parse Ipo Entity from JSON list to Data.frame
#'
#'@param data Returned fields card response in json list form for the entity
#'@return  a data.frame of parsed output
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' parseIpo(data$cards$fields)
#'
#'@import stringr
#'
parseIpo <- function(data) {
  # Identifier parsing
  ids <- parseIdentifier(data[["identifier"]], "identifier")

  # Simple parsing
  rank_ipo <- simpleParse(data[["rank_ipo"]])
  short_description <- simpleParse(data[["short_description"]])
  went_public_on <- simpleParse(data[["went_public_on"]])
  stock_full_symbol <- simpleParse(data[["stock_full_symbol"]])
  stock_exchange_symbol <- simpleParse(data[["stock_exchange_symbol"]])
  stock_symbol <- simpleParse(data[["stock_symbol"]])
  created_at <- simpleParse(data[["created_at"]])
  updated_at <- simpleParse(data[["updated_at"]])
  delisted_on <- simpleParse(data[["delisted_on"]])
  went_public_on <- simpleParse(data[["went_public_on"]])
  image_id <- simpleParse(data[["image_id"]])
  rank <- simpleParse(data[["rank"]])
  shares_outstanding <- simpleParse(data[["shares_outstanding"]])
  shares_sold <- simpleParse(data[["shares_sold"]])

  # Currency list parsing
  amount_raised <- currencyListParse(data[["amount_raised"]], "amount_raised")
  share_price <- currencyListParse(data[["share_price"]], "share_price")
  valuation <- currencyListParse(data[["valuation"]], "valuation")


  # Put into one dateframe
  df <- data.frame(cbind(  ids ,
                           rank,
                           rank_ipo,
                           short_description,
                           stock_symbol,
                           stock_full_symbol,
                           stock_exchange_symbol,
                           amount_raised,
                           share_price,
                           valuation,
                           shares_outstanding,
                           shares_sold,
                           image_id,
                           went_public_on,
                           delisted_on,
                           created_at,
                           updated_at

  ))

  # Adjust classes
  # as numeric
  df$rank <- as.numeric(df$rank)
  df$rank_ipo <- as.numeric(df$rank_ipo)
  df$shares_outstanding <- as.numeric(df$shares_outstanding)
  df$shares_sold <- as.numeric(df$shares_sold)
  df$valuation_value <- as.numeric(df$valuation_value)
  df$valuation_value_usd <- as.numeric(df$valuation_value_usd)
  df$amount_raised_value <- as.numeric(df$amount_raised_value)
  df$amount_raised_value_usd <- as.numeric(df$amount_raised_value_usd)
  df$share_price_value <- as.numeric(df$share_price_value)
  df$share_price_value_usd <- as.numeric(df$share_price_value_usd)

  # as date
  df$delisted_on <- as.Date(df$delisted_on)
  df$went_public_on <- as.Date(df$went_public_on)
  df$created_at <- as.Date(df$created_at)
  df$updated_at <- as.Date(df$updated_at)

  # Return dataframe
  return(df)
}