#'Function to construct currency type search conditions to be 'rbind'ed into one dataframe to be passed as conditions argument to any search function
#'
#'@param subject the field you are conditioning on e.g. 'location_identifiers'
#'@param verb the operator you want to use to connect 'subject' and 'object' e.g. includes, between.. A full list of all operators available can be obtained with the call 'getOperators()'
#'@param value integer or numeric value of the currency condition
#'@param currency of the value of the currency condition as a character e.g. "usd"
#'@return a data.frame row that can be easily 'rbind'ed to other search conditions to be passed in a searchForXXX(search_conditions) call to search for certain entities
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' searchConditionCurrency(subject = "money_raised", verb = "gte", value = 10000000L, currency = "usd")
#'
#'@import stringr
#'@import dplyr
#' @export
#'
searchConditionCurrency <- function(subject, verb, value, currency) {

  # Check class of currency
  if (class(currency) != "character") {
    stop("Currency must be of class 'character' e.g. 'usd'. Please try again.")
  }

  # Check that value is of type numeric or integer
  if (!class(value) %in% c("numeric", "integer")) {
    stop("Value must be of class numeric or integer.")
  }

  # String check to lower
  currency <- stringr::str_to_lower(currency)

  # Check that verb is one of the operators
  if (!verb %in% getOperators()$operators) {
    stop("Verb must be a valid operator. Call getOperators() to view them. Keep in mind that the operator must match the class of the subject.")
  }

  # Create object data.frame
  df <- data.frame(value = value, currency = currency)
  # Check classes to integer and character
  df$value <- as.integer(df$value)
  df$currency <- as.character(df$currency)

  # Return condition row data.frame
  return(searchCondition(subject = subject, verb = verb, object = df))
}