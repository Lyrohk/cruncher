#' Function to parse Press Reference Entity from JSON list to Data.frame
#'
#'@param data Returned fields card response in json list form for the entity
#'@return  a data.frame of parsed output
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' parsePressReference(data$cards$fields)
#'
#'
parsePressReference <- function(data) {
  # Identifier parsing
  ids <- parseIdentifier(data[["identifier"]], "identifier")

  # Simple parsing
  title <- simpleParse(data[["title"]])
  author <- simpleParse(data[["author"]])
  publisher <- simpleParse(data[["publisher"]])
  posted_on <- simpleParse(data[["posted_on"]])
  created_at <- simpleParse(data[["created_at"]])
  updated_at <- simpleParse(data[["updated_at"]])
  thumbnail_url <- simpleParse(data[["thumbnail_url"]])

  # List parsing
  url <- listParse(data[["url"]])
  activity_entities <- listParse(data[["activity_entities"]])

  # Put into one dateframe
  df <- data.frame(cbind(  ids,
                           title,
                           author,
                           publisher,
                           activity_entities,
                           posted_on,
                           created_at ,
                           updated_at, # Simple parsing done
                           url,
                           thumbnail_url
  ))

  # Adjust classes
  # as date
  df$posted_on <- as.Date(df$posted_on)
  df$created_at <- as.Date(df$created_at)
  df$updated_at <- as.Date(df$updated_at)

  # Return dataframe
  return(df)
}