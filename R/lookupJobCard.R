#' Function to lookup and paginate through a single card for a single job over the Entity Lookup API Endpoint
#'
#' Takes the entity id to lookup a card and paginate through the entities and parse them into a data.frame
#'
#'@param job_card card field of interest that will be returned. Only one please!
#'@param job_id UUID or permalink of the job you wish to look up
#'@param please_parse Logical. By default TRUE and will parse your data from a list of data.frames to a final data.frame with empty elements dropping out.
#'@return a data.frame
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' lookupJobCard(job_card = "card", job_id = "id")
#'
#'@export
#'
lookupJobCard <- function(job_card, job_id, please_parse = TRUE) {
  if (length(job_id) == 0) {
    stop("Please provide a valid job_id.")
  } else if (length(job_id) == 1) {
    # Lookup the cards for a single id
    return(lookupEntityCard(entity_card = job_card, entity_id = job_id, entity_path = "jobs"))
  } else {
    # Add duplicate and time check
    duplicateTimeCheck(job_id)

    # Check please_parse
    if (please_parse) {
      # Bind data into a final data.frame with those elements without data dropping out
      return( rbind_pages(silenceFun(lapply(job_id, lookupEntityCard, entity_card = job_card, entity_path = "jobs"))) )
    } else {
      # Return a list of (potentially empty) data.frames
      return(silenceFun(lapply(job_id, lookupEntityCard, entity_card = job_card, entity_path = "jobs")))
    }
  }
}