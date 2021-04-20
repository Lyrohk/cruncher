#' Function to parse Date to the provided level of precision
#'
#'@param data from json
#'@return parsed output
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' parseDateTime(data$cards$fields$someDate)
#'
#'@import stringr
#'
# Parse Date
parseDate <- function(data, var_name) {
  # Parse into two columns
  df <- data.frame("precision" = ifelse(length(data$precision) == 0,
                      NA,
                      data$precision),
  "value" = ifelse(length(data$value) == 0,
                  NA,
                  data$value) )
  # Rename
  colnames(df) <-
    c(paste(field, sep = "_", "precision"), paste(field, sep = "_", "value"))

  # Return data dataframe
  return(df)
}