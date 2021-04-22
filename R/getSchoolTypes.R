#' Helper function to return the revenue range dictionary
#'
#'@return dictionary
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' getSchoolTypes()
#'
#' @export
#'
getSchoolTypes <- function() {
  return(c(
    "for_profit_private" = "Private",
    "non_profit_private" = "Private (Non-Profit)",
    "public" = "Public"
  ))
}