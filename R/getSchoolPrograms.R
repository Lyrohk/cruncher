#' Helper function to return the school programs dictionary
#'
#'@return dictionary
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' getSchoolTypes()
#'
#' @export
#'
getSchoolPrograms <- function() {
  return(c(
    "bootcamp" = "Bootcamp",
    "community_college" = "Community College",
    "four_year_university" = "Four Year University",
    "graduate_university" = "Graduate University",
    "high_school" = "High School",
    "trade_school" = "Trade School",
    "two_year_university" = "Two Year University"
  ))
}