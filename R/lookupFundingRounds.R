#' Function to lookup a multiple funding rounds over the Entity Lookup API Endpoint
#'
#'@param funding_rounds UUID or permalink of the funding rounds you wish to look up
#'@param please_parse TRUE or FALSE. By default TRUE. If set to FALSE, it will return the data directly from the JSON, if set to TRUE, it will parse it into a data.frame object
#'@return either a data.frame (if please_parse = TRUE) or a list of lists (if please_parse = FALSE)
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' lookupFundingRounds(c("8a945939-18e0-cc9d-27b9-bf33817b2818", "d950d7a5-79ff-fb93-ca87-13386b0e2feb"))
#' lookupFundingRounds(list("8a945939-18e0-cc9d-27b9-bf33817b2818", "d950d7a5-79ff-fb93-ca87-13386b0e2feb"))
#'
#' @import dplyr
#' @export
#'
# Lookup multiple funding rounds
lookupFundingRounds <- function(funding_rounds, please_parse = TRUE) {
  # Check funding_rounds for class type
  if (!class(funding_rounds) %in% c("list", "character")) {
    stop("Please ensure that the funding_rounds are either a character vector or a list of character elements.")
  }
  
  # Check logical
  if (!class(please_parse) == "logical") {
    stop("Please use a TRUE or FALSE as input for please_parse.")
  }
  
  # Return requested output
  if (please_parse) {
    # Return a dataframe with each row having all the information about a certain organization
    df <- do.call(rbind.data.frame, lapply(X = funding_rounds, FUN = lookupFundingRound))
    # Filter out NA columns
    df <- df[colSums(!is.na(df)) > 0]
    # Return as data.frame
    return(df)
  } else {
    # Return the data converted from json as lists
    return(do.call(list, lapply(X = funding_rounds, FUN = lookupFundingRound, please_parse = FALSE)))
  }
}