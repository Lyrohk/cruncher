#' Function to parse Address Entity from JSON list to Data.frame
#'
#'@param data Returned fields card response in json list form for the entity
#'@return  a data.frame of parsed output
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' parseAddress(data$cards$fields)
#'
#'@import stringr
#'
# Parse Address Entity
parseAddress <- function(data) {

  # Identifier
  ids <- parseIdentifier(data[["identifier"]], "identifier")

  # Simple parsing
  postal_code <- simpleParse(data[["postal_code"]])
  street_1 <- simpleParse(data[["street_1"]])
  street_2 <- simpleParse(data[["street_2"]])
  created_at <- simpleParse(data[["created_at"]])
  updated_at <- simpleParse(data[["updated_at"]])

  # List parsing
  location_identifiers <- listParse(data[["location_identifiers"]])

  # Put into one dateframe
  df <- data.frame(cbind(  ids ,
                           postal_code,
                           street_1,
                           street_2,
                           location_identifiers,
                           created_at,
                           updated_at

  ))

  # Adjust classes
  # as date
  df$created_at <- as.Date(df$created_at)
  df$updated_at <- as.Date(df$updated_at)

  # Return dataframe
  return(df)
}