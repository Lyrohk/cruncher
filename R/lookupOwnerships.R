#' Function to lookup a multiple ownerships over the Entity Lookup API Endpoint
#'
#'@param ownerships UUID or permalink of the ownerships you wish to look up
#'@param please_parse Logical. By default TRUE. If set to FALSE, it will return the data directly from the JSON, if set to TRUE, it will parse it into a data.frame object
#'@param print_error Logical. Default to TRUE and thus if the request fails, an error message will be printed out.
#'@return either a data.frame (if please_parse = TRUE) or a list of lists (if please_parse = FALSE)
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' lookupOwnerships(c("f8fd68de-edf9-410a-9730-2f8f280a2cdd", "wayra-owns-wayra-uk-call--7e753e5c"))
#' lookupOwnerships(list("f8fd68de-edf9-410a-9730-2f8f280a2cdd", "wayra-owns-wayra-uk-call--7e753e5c"))
#'
#' @import dplyr
#' @export
#'
# Lookup multiple ownerships
lookupOwnerships <- function(ownerships, please_parse = TRUE, print_error = TRUE) {
  return(lookupEntities(entities = ownerships, path = "ownerships", please_parse = please_parse, print_error = print_error))
}