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
  
  # Check if it has content
  ifelse(length(simpleList) == 0,
         NA,
         paste(as.character(simpleList$value), collapse=", "))
}