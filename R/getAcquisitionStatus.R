#' Helper function to return the acquisition status dictionary
#'
#'@return dictionary
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' getAcquisitionStatus()
#'
#' @export
#'
getAcquisitionStatus <- function() {
  return(c("complete" = "Complete",
           "pending" = "Pending"))
}