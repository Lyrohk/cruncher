#' Helper function to parse a simple character or integer list
#'
#'@return parsed output
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' simpleParse("3")
#'
#'
# Simple Parse for a character, double, or integer element
simpleParse <- function(stringOrNumber) {
  # Check if it has content
  ifelse(length(stringOrNumber) == 0,
         NA,
         paste(as.character(stringOrNumber), collapse=", "))
}