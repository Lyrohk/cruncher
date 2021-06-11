#' Helper function to parse a identifier data into a dataframe
#'
#'@param field name that contains the identifier data
#'@return data.frame of uuid, permalink, value, and entity id
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' parseIdentifier("identifier")
#'
#'
# Identifier parsing function
parseIdentifier <- function(fields_data, field) {
  # Check if empty to return NA
  if (length(fields_data) == 0) {
    df <- data.frame(NA, NA, NA, NA)
    colnames(df) <- c(
      paste(field, sep = "_", "uuid"),
      paste(field, sep = "_", "value"),
      paste(field, sep = "_", "permalink"),
      paste(field, sep = "_", "entity_def_id")
    )
    return(df)
  }

  # Extra check for card lookup function to go to one level deeper
  if (length(fields_data) == 1) {
    fields_data <- fields_data[[1]]
  }

  # Get uuid, permalink, value, and entity id
  value <- as.data.frame(ifelse(length(fields_data$value) == 0,
                  NA,
                  paste(
                    as.character(fields_data$value), collapse = ", "
                  )))
  colnames(value) <- paste(field, sep = "_", "value")
  uuid <- as.data.frame(ifelse(length(fields_data$uuid) == 0,
                 NA,
                 paste(
                   as.character(fields_data$uuid), collapse = ", "
                 )))
  colnames(uuid) <- paste(field, sep = "_", "uuid")
  permalink <- as.data.frame(ifelse(length(fields_data$permalink) == 0,
                      NA,
                      paste(
                        as.character(fields_data$permalink), collapse = ", "
                      )))
  colnames(permalink) <- paste(field, sep = "_", "permalink")
  entity_def_id <- as.data.frame(ifelse(length(fields_data$entity_def_id) == 0,
                          NA,
                          paste(
                            as.character(fields_data$entity_def_id), collapse = ", "
                          )))
  colnames(entity_def_id) <-
    paste(field, sep = "_", "entity_def_id")
  identifier_df <- cbind(uuid, value, permalink, entity_def_id)
  # Return identifier dataframe
  return(identifier_df)
}
