#' Helper function to return the ownership types dictionary
#'
#'@return dictionary
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' getOwnershipTypes()
#'
#' @export
#'
getOwnershipTypes <- function() {
  return(c(
    "affiliated_company" = "Affiliated Company",
    "division" = "Division",
    "investment_arm" = "Investment Arm",
    "joint_venture" = "Joint Venture",
    "subsidiary" = "Subsidiary"
  ))
}

