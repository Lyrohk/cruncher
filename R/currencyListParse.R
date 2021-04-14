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
currencyListParse <- function(field) {
  # Check if it has content
  if(length(data[["cards"]][["fields"]][[field]]) == 0) {
    return(data.frame(value = NA, currency = NA, value_usd = NA)) 
  } else {
    # Get value, currency, and value_usd
    value <- data.frame(paste(as.character(data[["cards"]][["fields"]][[field]]$value), collapse=", "))
    colnames(value) <- paste(field, sep = "_", "value")
    currency <- data.frame(paste(as.character(data[["cards"]][["fields"]][[field]]$currency), collapse=", "))
    colnames(currency) <- paste(field, sep = "_", "currency")
    value_usd <- data.frame(paste(as.character(data[["cards"]][["fields"]][[field]]$value_usd), collapse=", "))
    colnames(value_usd) <- paste(field, sep = "_", "value_usd")
    # Return currency data.frame
    return(cbind(value, currency, value_usd)) 
  }
}