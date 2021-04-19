#' Function to parse Degree Entity from JSON list to Data.frame
#'
#'@param data Returned fields card response in json list form for the entity
#'@return  a data.frame of parsed output
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' parseDegree(data$cards$fields)
#'
#'@import stringr
#'
# Parse Degree Entity
parseDegree <- function(data) {

  # Identifier
  ids <- parseIdentifier(data[["identifier"]], "identifier")
  person_identifier <- parseIdentifier(data[["person_identifier"]], "person_identifier")
  school_identifier <- parseIdentifier(data[["school_identifier"]], "school_identifier")

  # Simple parsing
  subject <- simpleParse(data[["subject"]])
  type_name <- simpleParse(data[["type_name"]])
  created_at <- simpleParse(data[["created_at"]])
  updated_at <- simpleParse(data[["updated_at"]])

  # List parsing
  started_on <- listParse(data[["started_on"]])
  completed_on <- listParse(data[["completed_on"]])

  # Put into one dateframe
  df <- data.frame(cbind(  ids ,
                           person_identifier,
                           school_identifier,
                           subject,
                           type_name,
                           started_on,
                           completed_on,
                           created_at,
                           updated_at
  ))

  # Adjust classes
  # as date
  df$started_on <- as.Date(df$started_on,format="%Y-%m-%d")
  df$completed_on <- as.Date(df$completed_on,format="%Y-%m-%d")
  df$created_at <- as.Date(df$created_at)
  df$updated_at <- as.Date(df$updated_at)

  # Return dataframe
  return(df)
}