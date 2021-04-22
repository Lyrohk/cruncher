#' Helper function to return the job types dictionary
#'
#'@return dictionary
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' getJobTypes()
#'
#' @export
#'
getJobTypes <- function() {
  return(c(
    "advisor" = "Advisor",
    "board_member" = "Board Member",
    "board_observer" = "Board Observer",
    "employee" = "Non-Executive Employee",
    "executive" = "Executive"
  ))
}

