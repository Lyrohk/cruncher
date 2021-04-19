#' Function to parse Ownership Entity from JSON list to Data.frame
#'
#'@param data Returned fields card response in json list form for the entity
#'@return  a data.frame of parsed output
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' parseOwnership(data$cards$fields)
#'
#'@import stringr
#'
parseOwnership <- function(data) {
  # Identifier parsing
  ids <- parseIdentifier(data[["identifier"]], "identifier")
  ownee_identifier <- parseIdentifier(data[["ownee_identifier"]], "ownee_identifier")
  owner_identifier <- parseIdentifier(data[["owner_identifier"]], "owner_identifier")

  # Simple parsing
  rank <- simpleParse(data[["rank"]])
  created_at <- simpleParse(data[["created_at"]])
  updated_at <- simpleParse(data[["updated_at"]])
  ownership_type <- str_replace_all(simpleParse(data[["ownership_type"]]),
                                    pattern = c(
                                      "affiliated_company" = "Affiliated Company",
                                      "division" = "Division",
                                      "investment_arm" = "Investment Arm",
                                      "joint_venture" = "Joint Venture",
                                      "subsidiary" = "Subsidiary"
                                    ))

  # Put into one dateframe
  df <- data.frame(cbind(  ids ,
                           ownee_identifier,
                           owner_identifier,
                           ownership_type,
                           rank,
                           created_at,
                           updated_at
  ))

  # Adjust classes
  # as numeric
  df$rank <- as.numeric(df$rank)

  # as date
  df$created_at <- as.Date(df$created_at)
  df$updated_at <- as.Date(df$updated_at)

  # Return dataframe
  return(df)
}