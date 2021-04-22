#' Helper function to return the employee ranges dictionary
#'
#'@return dictionary
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' getEmployeeRanges()
#'
#' @export
#'
getEmployeeRanges <- function() {
  return(c(
    "c_00001_00010" = "1-10",
    "c_00011_00050" = "11-50",
    "c_00051_00100" = "51-100",
    "c_00101_00250" = "101-250",
    "c_00251_00500" = "251-500",
    "c_00501_01000" = "501-1000",
    "c_01001_05000" = "1001-5000",
    "c_05001_10000" = "5001-10000",
    "c_10001_max" = "10001+"
  ))
}

