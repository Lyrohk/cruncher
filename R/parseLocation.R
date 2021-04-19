#' Function to parse Location Entity from JSON list to Data.frame
#'
#'@param data Returned fields card response in json list form for the entity
#'@return  a data.frame of parsed output
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' parseLocation(data$cards$fields)
#'
#'
# Parse Location Entity
parseLocation <- function(data) {

  # Identifier
  ids <- parseIdentifier(data[["identifier"]], "identifier")

  # Simple parse character or numeric elements
  short_description <- simpleParse(data[["short_description"]])
  country_code <- simpleParse(data[["country_code"]])
  region_code <- simpleParse(data[["region_code"]])
  country_code_ext <- simpleParse(data[["country_code_ext"]])
  facet_ids <- simpleParse(data[["facet_ids"]])
  rank <- simpleParse(data[["rank"]])
  num_relationships <- simpleParse(data[["num_relationships"]])
  permalink_aliases <- simpleParse(data[["permalink_aliases"]])
  created_at <- simpleParse(data[["created_at"]])
  updated_at <- simpleParse( data[["updated_at"]])

  # List parse
  groups <- listParse(data[["groups"]])
  locations <- listParse(data[["locations"]])


  # Return as dataframe
  df <- data.frame(cbind(ids,
                         groups,
                         locations,
                         facet_ids,
                         short_description,
                         country_code,
                         country_code_ext,
                         region_code,
                         permalink_aliases,
                         rank,
                         num_relationships,
                         created_at,
                         updated_at
  ))

  # Convert numeric columns to numeric class from character
  df$rank <- as.numeric(df$rank)
  df$num_relationships <- as.numeric(df$num_relationships)

  # Convert to simple Date
  df$created_at <- as.Date(df$created_at)
  df$updated_at <- as.Date(df$updated_at)

  # Return dataframe
  return(df)
}