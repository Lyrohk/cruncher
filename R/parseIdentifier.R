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
  if(length(fields_data) == 0) {
    df <- data.frame(NA, NA, NA, NA) 
    colnames(df) <- c(paste(field, sep = "_", "uuid"), 
                      paste(field, sep = "_", "value"),
                      paste(field, sep = "_", "permalink"),
                      paste(field, sep = "_", "entity_def_id"))
    return(df)
  }
  
  # Get uuid, permalink, value, and entity id
  value <- data.frame(paste(as.character(fields_data$value), collapse=", "))
  colnames(value) <- paste(field, sep = "_", "value")
  uuid <- data.frame(paste(as.character(fields_data$uuid), collapse=", "))
  colnames(uuid) <- paste(field, sep = "_", "uuid")
  permalink <- data.frame(paste(as.character(fields_data$permalink), collapse=", "))
  colnames(permalink) <- paste(field, sep = "_", "permalink")
  entity_def_id <- data.frame(paste(as.character(fields_data$entity_def_id), collapse=", "))
  colnames(entity_def_id) <- paste(field, sep = "_", "entity_def_id")
  identifier_df <- cbind(uuid, value, permalink, entity_def_id)
  # Return identifier dataframe
  return(identifier_df)
}

