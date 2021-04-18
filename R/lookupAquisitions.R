#' Function to lookup a multiple acquisitions over the Entity Lookup API Endpoint
#'
#'@param acquisitions UUID or permalink of the acquisitions you wish to look up
#'@param please_parse TRUE or FALSE. By default TRUE. If set to FALSE, it will return the data directly from the JSON, if set to TRUE, it will parse it into a data.frame object
#'@param cut_na_cols If set to TRUE (default), the function will cut out empty columns filled with NAs
#'@return either a data.frame (if please_parse = TRUE) or a list of lists (if please_parse = FALSE)
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' lookupAquisitions(c("c62807f2-3487-f63b-e7fc-559b752ed44c", "419cabe1-4d71-5f64-3e9b-c9d0fb445cca"))
#' lookupAquisitions(list("c62807f2-3487-f63b-e7fc-559b752ed44c", "419cabe1-4d71-5f64-3e9b-c9d0fb445cca"))
#'
#' @import dplyr
#' @export
#'
# Lookup multiple acquisitions
lookupAquisitions <- function(acquisitions, please_parse = TRUE, cut_na_cols = TRUE) {
  # Check acquisitions for class type
  if (!class(acquisitions) %in% c("list", "character")) {
    stop("Please ensure that the acquisitions are either a character vector or a list of character elements.")
  }
  
  # Check logical
  if (!class(please_parse) == "logical") {
    stop("Please use a TRUE or FALSE as input for please_parse.")
  }
  
  # Return requested output
  if (please_parse) {
    # Return a dataframe with each row having all the information about a certain organization
    df <- do.call(rbind.data.frame, lapply(X = acquisitions, FUN = lookUpAquisition))
    if (cut_na_cols) {
      # Filter out na columns
      return(df[colSums(!is.na(df)) > 0])
    } else {
      return(df)
    }
  } else {
    # Return the data converted from json as lists
    return(do.call(list, lapply(X = acquisitions, FUN = lookUpAquisition, please_parse = FALSE)))
  }
}