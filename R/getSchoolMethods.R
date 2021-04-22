#' Helper function to return the school method dictionary
#'
#'@return dictionary
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' getSchoolMethods()
#'
#' @export
#'
getSchoolMethods <- function() {
  return(c(
    "on_compus" = "On Campus",
    "online" = "Online",
    "online_and_on_campus" = "Online and On Campus"
  ))
}