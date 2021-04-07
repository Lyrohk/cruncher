#' Helper function to parse a currency element
#'
#'@return parsed output
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' currencyListParse(CurrencyElement)
#'
#'
# Currency Parse for lists with value and value_usd
currencyListParse <- function(simpleList) {
  # Check if it has content
  ifelse(length(simpleList) == 0,
         NA,
         paste(as.character(simpleList$value_usd), collapse=", "))
}