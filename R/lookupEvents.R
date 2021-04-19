#' Function to lookup multiple events over the Entity Lookup API Endpoint
#'
#'@param events UUID or permalink of the events you wish to look up
#'@param please_parse TRUE or FALSE. By default TRUE. If set to FALSE, it will return the data directly from the JSON, if set to TRUE, it will parse it into a data.frame object
#'@return either a data.frame (if please_parse = TRUE) or a list of lists (if please_parse = FALSE)
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' lookupEvents(c("6a21f93a-3cae-02ac-44c7-c166c0ed50fa", "135a927a-b238-037e-50e2-d3a2e3511ed2"))
#' lookupEvents(list("6a21f93a-3cae-02ac-44c7-c166c0ed50fa", "135a927a-b238-037e-50e2-d3a2e3511ed2"))
#'
#' @import dplyr
#' @export
#'
lookupEvents <- function(events, please_parse = TRUE) {
  return(lookupEntities(entities = events, path = "events", please_parse))
}
