#' Function to lookup multiple investments over the Entity Lookup API Endpoint
#'
#'@param investmentss UUID or permalink of the investments you wish to look up
#'@param please_parse TRUE or FALSE. By default TRUE. If set to FALSE, it will return the data directly from the JSON, if set to TRUE, it will parse it into a data.frame object
#'@return either a data.frame (if please_parse = TRUE) or a list of lists (if please_parse = FALSE)
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' lookupInvestments(c("524986f0-3049-54a4-fa72-f60897a5e61d", "6556ab92-6465-25aa-1ffc-7f8b4b09a476"))
#' lookupInvestments(list("524986f0-3049-54a4-fa72-f60897a5e61d", "6556ab92-6465-25aa-1ffc-7f8b4b09a476"))
#'
#' @import dplyr
#' @export
#'
# Lookup multiple investments
lookupInvestments <- function(investments, please_parse = TRUE) {
  # Check investments for class type
  if (!class(investments) %in% c("list", "character")) {
    stop("Please ensure that the investments are either a character vector or a list of character elements.")
  }
  
  # Check logical
  if (!class(please_parse) == "logical") {
    stop("Please use a TRUE or FALSE as input for please_parse.")
  }
  
  # Return requested output
  if (please_parse) {
    # Return a dataframe with each row having all the information about a certain investments
    df <- do.call(rbind.data.frame, lapply(X = investments, FUN = lookupInvestment))
    # Filter out NA columns
    df <- df[colSums(!is.na(df)) > 0]
    # Return as data.frame
    return(df)
  } else {
    # Return the data converted from json as lists
    return(do.call(list, lapply(X = investments, FUN = lookupInvestment, please_parse = FALSE)))
  }
}