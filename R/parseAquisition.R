#' Function to parse Aquisition Entity from JSON list to Data.frame
#'
#'@param data Returned fields card response in json list form for the entity
#'@return  a data.frame of parsed output
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' parseAquisition(data$cards$fields)
#'
#'@import stringr
#'
# Parse Aquisition Entity
parseAquisition <- function(data) {

  # Identifier
  ids <- parseIdentifier(data[["identifier"]], "identifier")
  acquirer_identifier <- parseIdentifier(data[["acquirer_identifier"]], "acquirer_identifier")
  acquiree_identifier <- parseIdentifier(data[["acquiree_identifier"]], "acquiree_identifier")

  # Simple parsing
  short_description <- simpleParse(data[["short_description"]])
  acquirer_short_description <- simpleParse(data[["acquirer_short_description"]])
  acquiree_short_description <- simpleParse(data[["acquiree_short_description"]])
  # Convert
  status <- str_replace_all(simpleParse(data[["status"]]),
                            pattern = getAcquisitionStatus())
  rank <- simpleParse(data[["rank"]])
  rank_acquisition <- simpleParse(data[["rank_acquisition"]])
  # Apply revenue converter
  acquiree_revenue_range <- convertRevenueRange(simpleParse(data[["acquiree_revenue_range"]]))
  acquirer_revenue_range <- convertRevenueRange(simpleParse(data[["acquirer_revenue_range"]]))
  # Apply funding stage converter
  acquirer_funding_stage <- convertFundingStage(simpleParse(data[["acquirer_funding_stage"]]))
  # Apply funding type converter
  acquiree_last_funding_type <- convertFundingType(simpleParse(data[["acquiree_last_funding_type"]]))
  # Acquisition type dict
  acquisition_type <- str_replace_all(simpleParse(data[["acquisition_type"]]), pattern = getAcquisitionTypes())
  # Converted
  disposition_of_acquired <- str_replace_all(simpleParse(data[["disposition_of_acquired"]]),
                                             pattern = c("combined" = "Combined",
                                                         "division" = "Division",
                                                         "product" = "Product",
                                                         "separate_entity" = "Separate Entity",
                                                         "subsidiary" = "Subsidiary"))
  acquiree_num_funding_rounds <- simpleParse(data[["acquiree_num_funding_rounds"]])
  acquirer_num_funding_rounds <- simpleParse(data[["acquirer_num_funding_rounds"]])
  num_relationships <- simpleParse(data[["num_relationships"]])
  # Apply terms converter
  terms <- convertTerms(simpleParse(data[["terms"]]))
  updated_at <- simpleParse(data[["updated_at"]])
  created_at <- simpleParse(data[["created_at"]])

  # List parsing
  completed_on <- listParse(data[["completed_on"]])
  announced_on <- listParse(data[["announced_on"]])
  acquirer_categories <- listParse(data[["acquirer_categories"]])
  acquiree_categories <- listParse(data[["acquiree_categories"]])
  acquirer_locations <- listParse(data[["acquirer_locations"]])
  acquiree_locations <- listParse(data[["acquiree_locations"]])

  # Currency list parsing
  acquirer_funding_total <- currencyListParse(data[["acquirer_funding_total"]], "acquirer_funding_total")
  acquiree_funding_total <- currencyListParse(data[["acquiree_funding_total"]], "acquiree_funding_total")
  price <- currencyListParse(data[["price"]], "price")

  # Put into one dateframe
  df <- data.frame(cbind(  ids ,
                           short_description ,
                           acquirer_short_description ,
                           acquiree_short_description,
                           status ,
                           rank,
                           rank_acquisition ,
                           acquiree_revenue_range ,
                           acquirer_revenue_range ,
                           acquirer_funding_stage ,
                           acquiree_last_funding_type ,
                           acquisition_type ,
                           disposition_of_acquired,
                           acquiree_num_funding_rounds ,
                           acquirer_num_funding_rounds ,
                           num_relationships,
                           created_at ,
                           terms ,
                           updated_at, # Simple parsing done
                           announced_on ,
                           completed_on,
                           acquiree_identifier ,
                           acquirer_categories ,
                           acquiree_categories ,
                           acquirer_locations ,
                           acquiree_locations ,
                           # Currency list parsing
                           acquirer_funding_total,
                           acquiree_funding_total ,
                           price

  ))

  # Adjust classes
  # as numeric (currencies)
  df$acquirer_funding_total_value <- as.numeric(df$acquirer_funding_total_value)
  df$acquirer_funding_total_value_usd <- as.numeric(df$acquirer_funding_total_value_usd)
  df$acquiree_funding_total_value <- as.numeric(df$acquiree_funding_total_value)
  df$acquiree_funding_total_value_usd <- as.numeric(df$acquiree_funding_total_value_usd)
  df$price_value <- as.numeric(df$price_value)
  df$price_value_usd <- as.numeric(df$price_value_usd)

  # as numeric
  df$acquiree_num_funding_rounds <- as.numeric(df$acquiree_num_funding_rounds)
  df$acquirer_num_funding_rounds <- as.numeric(df$acquirer_num_funding_rounds)
  df$rank_acquisition <- as.numeric(df$rank_acquisition)
  df$rank <- as.numeric(df$rank)
  df$num_relationships <- as.numeric(df$num_relationships)

  # as date
  df$announced_on <- as.Date(df$announced_on)
  df$created_at <- as.Date(df$created_at)
  df$updated_at <- as.Date(df$updated_at)

  # Return dataframe
  return(df)
}