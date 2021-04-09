#' Function to check and help out with uuid/permalink of an entity uuid to make it consistent
#' 
#' It will string manipulate the input to lower case, 
#' trim whitespace at beginning and end, 
#' and replace spaces with hyphens so that it can be passed in to an entity lookup function
#' 
#'@param input Original input string as uuid/permalink to the function
#'@return the string manipulated output
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' autocomplete(collection_ids = "categories", query = "Artificial Intelligence", limit = 5)
#'
#' @import stringr
#' @import dplyr
#'
# Function to string manipulate entity uuid
entityIdCheck <- function(input) {
  # Check if lower
  input <- input %>%
    str_to_lower() %>%
    str_trim() %>%
    str_replace_all(" ", "-")
  # Return input
  input
}