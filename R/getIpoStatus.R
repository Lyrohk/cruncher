#' Helper function to return the ipo status dictionary
#'
#'@return dictionary
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' getIpoStatus()
#'
#' @export
#'
getIpoStatus <- function() {
  return(c(
    "delisted" = "Delisted",
    "private" = "Private",
    "public" = "Public"
  ))
}

