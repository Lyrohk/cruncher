#' Helper function to convert funding types 
#'
#'@return converted funding types
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' convertFundingType()
#'
#' @import stringr
#'
# Helper function to get funding type dictionary
convertFundingType <- function(funding_type) {
  return(str_replace_all(funding_type, getFundingTypes()))
}
