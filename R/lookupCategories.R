#' Function to lookup multiple categories over the Entity Lookup API Endpoint
#'
#'@param persons UUID or permalink of the categories you wish to look up
#'@param please_parse TRUE or FALSE. By default TRUE. If set to FALSE, it will return the data directly from the JSON, if set to TRUE, it will parse it into a data.frame object
#'@return either a data.frame (if please_parse = TRUE) or a list of lists (if please_parse = FALSE)
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' lookUpCategories(c("	f9b14a15-5517-8f38-0562-729ebb54dfdb", "e8804a82-8603-d2e1-c3b5-d241dd8030b9"))
#' lookUpCategories(list("	f9b14a15-5517-8f38-0562-729ebb54dfdb", "e8804a82-8603-d2e1-c3b5-d241dd8030b9"))
#'
#' @import dplyr
#' @export
#'
# Lookup multiple category groups
lookUpCategories <- function(categories, please_parse = TRUE) {
  # Check organizations for class type
  if (!class(categories) %in% c("list", "character")) {
    stop("Please ensure that the organizations are either a character vector or a list of character elements.")
  }
  
  # Check logical
  if (!class(please_parse) == "logical") {
    stop("Please use a TRUE or FALSE as input for please_parse.")
  }
  
  # Return requested output
  if (please_parse) {
    # Return a dataframe with each row having all the information about a certain organization
    df <- do.call(rbind.data.frame, lapply(X = categories, FUN = lookUpPerson))
    # Filter out NA columns
    df <- df[colSums(!is.na(df)) > 0]
    # Return as data.frame
    return(df)
  } else {
    # Return the data converted from json as lists
    return(do.call(list, lapply(X = persons, FUN = categories, please_parse = FALSE)))
  }
}