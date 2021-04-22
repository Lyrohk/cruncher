#' Helper function to return the event appearance types dictionary
#'
#'@return dictionary
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' getEventAppearanceTypes()
#'
#' @export
#'
getEventAppearanceTypes <- function() {
  return(c(
    "contestant" = "Contestant",
    "exhibitor" = "Exhibitor",
    "organizer" = "Organizer",
    "speaker" = "Speaker",
    "sponsor" = "Sponsor"
  ))
}

