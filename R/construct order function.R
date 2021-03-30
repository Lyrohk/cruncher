#' Function to use set up the JSON order set within the JSON query body for the Search API endpoint of Crunchbase API
#'
#'@param order_by Field name by which the search response should be ordered
#'@param sort_direction Order direction, either "asc" for ascending or "desc" for descending. The default is "asc"
#'@return an order object that can be passed in as the order parameter in the construct query function to make a Search API call
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
  
  
  # Construct list
  inner_list <- vector(mode = "list", length = 2)
  names(inner_list) <- c("field_id", "sort")
  inner_list[[1]] <- order_by
  inner_list[[2]] <- sort_direction
  
  # Return list
  order <- vector(mode = "list", length = 1)
  order[[1]] <- inner_list
  order
}
