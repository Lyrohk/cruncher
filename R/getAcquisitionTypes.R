#' Helper function to return the acquisition types dictionary
#'
#'@return dictionary
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' getAcquisitionTypes()
#'
#' @export
#'
getAcquisitionTypes <- function() {
  return(c("acquihire" = "Acquihire",
           "acquisition" = "Acquisition",
           "lbo" = "Leveraged Buyout",
           "management_buyout" = "Management Buyout",
           "merge" = "Merger"))
  }