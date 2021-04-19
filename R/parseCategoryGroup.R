#' Function to parse Category Group Entity from JSON list to Data.frame
#'
#'@param data Returned fields card response in json list form for the entity
#'@return  a data.frame of parsed output
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' parseCategoryGroup(data$cards$fields)
#'
#'
# Parse Category Entity
parseCategoryGroup <- function(data) {

  # Identifier
  ids <- parseIdentifier(data[["identifier"]], "identifier")

  # Simple parse character or numeric elements
  rank <- simpleParse(data[["rank"]])
  created_at <- simpleParse(data[["created_at"]])
  updated_at <- simpleParse( data[["updated_at"]])
  # List parse
  categories <- listParse(data[["categories"]])


  # Return as dataframe
  df <- data.frame(cbind(ids,
                         categories,
                         rank,
                         created_at,
                         updated_at
  ))

  # Convert numeric columns to numeric class from character
  df$rank <- as.numeric(df$rank)

  # Convert to simple Date
  df$created_at <- as.Date(df$created_at)
  df$updated_at <- as.Date(df$updated_at)

  # Return dataframe
  return(df)
}