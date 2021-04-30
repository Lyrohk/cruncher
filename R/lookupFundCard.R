#' Function to lookup and paginate through a single card for a single fund over the Entity Lookup API Endpoint
#'
#' Takes the entity id to lookup a card and paginate through the entities and parse them into a data.frame
#'
#'@param fund_card card field of interest that will be returned. Only one please!
#'@param fund_id UUID or permalink of the fund you wish to look up
#'@param please_parse Logical. By default TRUE and will parse your data from a list of data.frames to a final data.frame with empty elements dropping out.
#'@return a data.frame
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' lookupFundCard(fund_card = "card", fund_id = "id")
#'
#'@export
#'
lookupFundCard <- function(fund_card, fund_id, please_parse = TRUE) {
  if (length(fund_id) == 0) {
    stop("Please provide a valid fund_id.")
  } else if (length(fund_id) == 1) {
    # Lookup the cards for a single id
    return(lookupEntityCard(entity_card = fund_card, entity_id = fund_id, entity_path = "funds"))
  } else {
    # Add duplicate and time check
    duplicateTimeCheck(fund_id)

    # Check please_parse
    if (please_parse) {
      # Bind data into a final data.frame with those elements without data dropping out
      return( rbind_pages(silenceFun(lapply(fund_id, lookupEntityCard, entity_card = fund_card, entity_path = "funds"))) )
    } else {
      # Return a list of (potentially empty) data.frames
      return(silenceFun(lapply(fund_id, lookupEntityCard, entity_card = fund_card, entity_path = "funds")))
    }
  }
}