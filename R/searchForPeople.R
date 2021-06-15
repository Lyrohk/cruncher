#'Function to search for people that fulfill pre-specified search conditions over the Search API
#'
#'@param search_conditions the search condition data.frame that the people have to fulfill
#'@param order_by (Optional) field by which to sort the results
#'@param sort_by (Optional) sort direction to sort the order field by. Either 'asc' or 'desc'.
#'@param result_limit (Optional) numeric limit of the number of results you'd like to have. By default, all results will be returned to you.
#'@param uuids_only (Optional) Logical if you want to have only the uuids of the searched for entities returned. By default, after the uuids are retrieved, the information for all entities will be looked up over the entity lookup API endpoint, which takes longer.
#'@param precise (Optional) Logical Default to TRUE. For a large number of results, you may want to set precise to FALSE as realtime updates may make the function otherwise run on indefinitely. When you set precise to FALSE, the last iteration set of results will not be returned. You can modify this parameter with the argument iteration.
#'@param iteration (Optional) numeric iteration step. Default to 1000L, max 2000L. The last iteration will be discarded if you set precise to FALSE. Otherwise, this parameter won't affect you.
#'@return a data.frame
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' searchForPeople(rbind(searchCondition(subject = "location_identifiers", verb = "includes", object = c("germany")),
#'                        searchCondition(subject = "categories", verb = "includes", object = c("artifial_intelligence", "data_mining") )))
#'
#' @import httr
#' @import jsonlite
#' @import stringr
#' @import dplyr
#' @export
#'
searchForPeople <- function(search_conditions, order_by = "identifier", sort_by = "asc", result_limit = NA, uuids_only = F, precise = T, iteration = 1000L) {
  # Search for the entity on the path with the provided search conditions
  return(searchForEntity(path = "people", conditions = search_conditions, order_by = order_by, sort_by = sort_by, result_limit = result_limit, uuids_only = uuids_only, precise = precise, iter = iteration))
}