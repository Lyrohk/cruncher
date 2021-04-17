#' Helper function to convert renvenue range to human readable numbers
#'
#'@return Human understandable format of the revenue range e.g. "$1M to $10M"
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' convertRevenue("r_00001000")
#'
#'
# Revenue Converter Function
convertRevenueRange <- function(rev_range) {
  # Check if missing
  if (rev_range == "" | is.na(rev_range)) {
    return(NA)
  } else {
    # Convert the revenue ranges if they are recognized
    if (rev_range == "r_00000000") {
      return("Less than $1M")
    } else if (rev_range == "r_00001000") {
      return("$1M to $10M")
    } else if (rev_range == "r_00010000") {
      return("$10M to $50M")
    } else if (rev_range == "r_00050000") {
      return("$50M to $100M")
    } else if (rev_range == "r_00100000") {
      return("$100M to $500M")
    } else if (rev_range == "r_00500000") {
      return("$500M to $1B")
    } else if (rev_range == "r_01000000") {
      return("$1B to $10B")
    } else if (rev_range == "r_10000000") {
      return("$10B+")
    } else {
      # Keep it as is
      return(rev_range)
    }
  }
}