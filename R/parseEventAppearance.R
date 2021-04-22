#' Function to parse Event Appearance Entity from JSON list to Data.frame
#'
#'@param data Returned fields card response in json list form for the entity
#'@return  a data.frame of parsed output
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' parseInvestment(data$cards$fields)
#'
#'@import stringr
#'
parseEventAppearance <- function(data) {
  # Identifier parsing
  ids <- parseIdentifier(data[["identifier"]], "identifier")
  event_identifier <- parseIdentifier(data[["event_identifier"]], "event_identifier")
  participant_identifier <- parseIdentifier(data[["participant_identifier"]], "participant_identifier")

  # Simple parsing
  appearance_type <- str_replace_all(simpleParse(data[["appearance_type"]]),
                                     pattern = getEventAppearanceTypes())
  rank <- simpleParse(data[["rank"]])
  short_description <- simpleParse(data[["short_description"]])
  starts_on <- simpleParse(data[["event_starts_on"]])
  created_at <- simpleParse(data[["created_at"]])
  updated_at <- simpleParse(data[["updated_at"]])

  # List parsing
  location <- listParse(data[["event_location_identifiers"]])



  # Put into one dateframe
  df <- data.frame(cbind(  ids,
                           event_identifier,
                           participant_identifier,
                           short_description,
                           appearance_type,
                           rank,
                           starts_on,
                           created_at ,
                           updated_at, # Simple parsing done
                           location

  ))

  # Adjust classes
  # as numeric
  df$rank <- as.numeric(df$rank)

  # as date
  df$starts_on <- as.Date(df$starts_on)
  df$created_at <- as.Date(df$created_at)
  df$updated_at <- as.Date(df$updated_at)

  # Return dataframe
  return(df)
}