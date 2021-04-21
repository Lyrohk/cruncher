#'Function to search for ipos that fulfill pre-specified search conditions over the Search API
#'
#'@param search_conditions the search condition data.frame that the entities have to fulfill
#'@param order_by (Optional) field by which to sort the results
#'@param sort_by (Optional) sort direction to sort the order field by. Either 'asc' or 'desc'.
#'@param result_limit (Optional) numeric limit of the number of results you'd like to have. By default, all results will be returned to you.
#'@param uuids_only (Optional) Logical if you want to have only the uuids of the searched for entities returned. By default, after the uuids are retrieved, the information for all entities will be looked up over the entity lookup API endpoint, which takes longer.
#'@return a data.frame
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' searchForIpos(rbind(searchCondition1,searchCondition2))
#'
#' @export
#'
searchForIpos <- function(search_conditions, order_by = "identifier", sort_by = "asc", result_limit = NA, uuids_only = F) {
  # Search for the entity on the path with the provided search conditions
  return(searchForEntity(path = "ipos", conditions = search_conditions, order_by = order_by, sort_by = sort_by, result_limit = result_limit, uuids_only = uuids_only))
}