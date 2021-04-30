#' Function to lookup and paginate through a single card for a single job over the Entity Lookup API Endpoint
#'
#' Takes the entity id to lookup a card and paginate through the entities and parse them into a data.frame
#'
#'@param job_card card field of interest that will be returned. Only one please!
#'@param job_id UUID or permalink of the job you wish to look up
#'@return a data.frame
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' lookupJobCard(job_card = "card", job_id = "id")
#'
#'@export
#'
lookupJobCard <- function(job_card, job_id) {
  if (length(job_id) == 0) {
    stop("Please provide a valid job_id.")
  } else if (length(job_id) == 1) {
    # Lookup the cards for a single id
    return(lookupEntityCard(entity_card = job_card, entity_id = job_id, entity_path = "jobs"))
  } else {
    # Add duplicate and time check
    duplicateTimeCheck(job_id)

    # There are multiple ids
    return(silenceFun(lapply(job_id, lookupEntityCard, entity_card = job_card, entity_path = "jobs")))
  }
}