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
#' 
# Helper function to view all operators possible
getOperators <- function() {
  # Make data.frame of all operators with description
  return(data.frame(
    description =
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
      ),
    operators = c(
      "blank",
      "eq",
      "not_eq",
      "gt",
      "gte",
      "lt",
      "lte",
      "starts",
      "contains",
      "between",
      "includes",
      "not_includes",
      "includes_all",
      "not_includes_all"
    )
  ))
}