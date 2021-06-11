#' Helper function to parse a simple list with values (data.frame)
#'
#'@return parsed output
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' listParse(DataFrameWithValueColumn)
#'
#'
# List Parser for simple list and dataframe
listParse <- function(simpleList) {
  # Convert to list
  simpleList <- as.list(simpleList)

  # Return NA is length 0
  if (length(simpleList) == 0) {
    return(NA)
  } else {

    # Check for card lookup length 1 and go one level deeper
    if (class(simpleList[[1]]) == "data.frame") {
      simpleList <- as.list(simpleList[[1]])
    }

    # Return collapsed comma separated value output
    return(paste(as.character(simpleList$value), collapse=", "))

  }
}