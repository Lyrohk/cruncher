#' Function to lookup multiple jobs over the Entity Lookup API Endpoint
#'
#'@param persons UUID or permalink of the jobs you wish to look up
#'@param please_parse TRUE or FALSE. By default TRUE. If set to FALSE, it will return the data directly from the JSON, if set to TRUE, it will parse it into a data.frame object
#'@return either a data.frame (if please_parse = TRUE) or a list of lists (if please_parse = FALSE)
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' lookupJobs(c("uuid1", "uuid2"))
#' lookupJobs(list("uuid1", "uuid2"))
#'
#' @import dplyr
#' @export
#'
# Lookup multiple jobs
lookupJobs <- function(jobs, please_parse = TRUE) {
  return(lookupEntities(entities = jobs, path = "jobs", please_parse = please_parse))
}