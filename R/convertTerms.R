#' Helper function to convert terms to human readable numbers
#'
#'@return Human understandable format of the terms e.g. "Cash & Stock"
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' convertTerms("cash")
#'
#'
# Terms Converter Function
convertTerms <- function(term) {
  # Check if missing
  if (term == "" | is.na(term)) {
    return(NA)
  } else {
    # Convert the terms if they are recognized
    if (term == "cash") {
      return("Cash")
    } else if (term == "cash_and_stock") {
      return("Cash & Stock")
    } else if (term == "stock") {
      return("Stock")
    } else {
      # Keep it as is
      return(term)
    }
  }
}