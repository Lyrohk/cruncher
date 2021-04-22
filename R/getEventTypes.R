#' Helper function to return the event types dictionary
#'
#'@return dictionary
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' getEventTypes()
#'
#' @export
#'
getEventTypes <- function() {
  return(c(
    "class" = "Class",
    "competition" = "Competition",
    "conference" = "Conference",
    "demo_day" = "Demo Day",
    "expo" = "Expo",
    "festival" = "Festival",
    "hackathon" = "Hackathon",
    "meetup" = "Meetup",
    "networking" = "Networking",
    "other" = "Other",
    "seminar" = "Seminar",
    "virtual" = "Virtual",
    "webinar" = "Webinar"
  ))
}

