#' Helper function to return all valid operators
#'
#'@return all valid operators
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' getOperators()
#'
#' @export
# Helper function to view all operators possible
getOperators <- function() {
  # Make list of all operators
  operators <- vector(mode = "list", length = 14)
  names(operators) <-
    c(
      "Blank",
      "Equal",
      "Not equal",
      "Greater than",
      "Greater than or equal",
      "Less than",
      "Less than or equal",
      "Starts",
      "Contains",
      "Between",
      "Includes",
      "Does not include",
      "Includes all",
      "Does not include all"
    )
  operators[[1]] <- "blank"
  operators[[2]] <- "eq"
  operators[[3]] <- "not_eq"
  operators[[4]] <- "gt"
  operators[[5]] <- "gte"
  operators[[6]] <- "lt"
  operators[[7]] <- "lte"
  operators[[8]] <- "starts"
  operators[[9]] <- "contains"
  operators[[10]] <- "between"
  operators[[11]] <- "includes"
  operators[[12]] <- "not_includes"
  operators[[13]] <- "includes_all"
  operators[[14]] <- "not_includes_all"
  
  # Print this list
  print(operators)
}