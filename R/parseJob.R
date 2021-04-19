#' Function to parse Job Entity from JSON list to Data.frame
#'
#'@param data Returned fields card response in json list form for the entity
#'@return  a data.frame of parsed output
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' parseJob(data$cards$fields)
#'
#'@import stringr
#'
# Parse Job Entity
parseJob <- function(data) {

  # Identifier
  ids <- parseIdentifier(data[["identifier"]], "identifier")
  person_identifier <- parseIdentifier(data[["person_identifier"]], "person_identifier")
  organization_identifier <- parseIdentifier(data[["organization_identifier"]], "organization_identifier")

  # Simple parsing
  rank <- simpleParse(data[["rank"]])
  employee_featured_order <- simpleParse(data[["employee_featured_order"]])
  is_current <- simpleParse(data[["is_current"]])
  short_description <- simpleParse(data[["short_description"]])
  title <- simpleParse(data[["title"]])
  job_type <- str_replace_all(simpleParse(data[["job_type"]]),
                              pattern = c(
                                "advisor" = "Advisor",
                                "board_member" = "Board Member",
                                "board_observer" = "Board Observer",
                                "employee" = "Non-Executive Employee",
                                "executive" = "Executive"
                              ))


  created_at <- simpleParse(data[["created_at"]])
  updated_at <- simpleParse(data[["updated_at"]])

  # List parsing
  person <- listParse(data[["person_identifier"]])
  organization <- listParse(data[["organization_identifier"]])
  started_on <- listParse(data[["started_on"]])
  ended_on <- listParse(data[["ended_on"]])

  # Put into one dateframe
  df <- data.frame(cbind(  ids ,
                           person_identifier,
                           organization_identifier,
                           title ,
                           is_current,
                           short_description,
                           job_type,
                           rank,
                           employee_featured_order,
                           created_at,
                           updated_at,
                           person,
                           organization,
                           started_on,
                           ended_on

  ))

  # Adjust classes
  # as numeric
  df$rank <- as.numeric(df$rank)
  df$employee_featured_order <- as.numeric(df$employee_featured_order)
  # as date
  df$started_on <- as.Date(df$started_on,format="%Y-%m-%d")
  df$ended_on <- as.Date(df$ended_on,format="%Y-%m-%d")
  df$created_at <- as.Date(df$created_at)
  df$updated_at <- as.Date(df$updated_at)
  # as logical
  df$is_current <- as.logical(df$is_current)

  # Return dataframe
  return(df)
}