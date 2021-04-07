#' Function to use set up the JSON order set within the JSON query body for the Search API endpoint of Crunchbase API
#'
#'@param order_by Field name by which the search response should be ordered
#'@param sort_direction Order direction, either "asc" for ascending or "desc" for descending. The default is "asc"
#'@return an order data.frame that can be passed in as the order parameter in the construct query function to make a Search API call
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' make_order(order_by = "founded_on", sort_direction = "desc")
#' make_order(order_by = "rank_org")
#'
#' @import dplyr
#' @export
#'
# Hidden function to make order
make_order <- function(order_by, sort_direction = "asc") {
  # Check that sort_direction is either asc or desc else stop early
  if (!sort_direction %in% c("asc", "desc")) {
    stop("Sort direction must be either 'asc' for ascending or 'desc' for descending.")
  }
  
  # Construct dataframe
  data.frame(field_id = order_by, 
             sort = sort_direction)
}
