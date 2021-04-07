#' Function to construct JSON body request to pass in for the Search API of the Crunchbase API
#'
#'@param fields Field names which will be returned as columns
#'@param order_by Field name by which the response should be ordered
#'@param sort_direction Order direction, either "asc" for ascending or "desc" for descending. The default is "asc"
#'@param query JSON of query conditions, each can be individuall constructed with the predicate function
#'@param limit Number of rows to be returned, default 100, max 2000, min 1
#'@param before_id For paginating through responses, use this to go back to the entries before this uuid
#'@param after_id For paginating through responses, use this to go forward to the entries after uuid
#'@return an list object that can be passed in as the JSON body parameter in the Search API call once converted to JSON
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' make_query(fields = "founded_on,locations", 
#' order_by = "founded_on", 
#' sort_direction = "desc",
#' query = q,
#' limit = 200)
#'
#' @import dplyr
#' @export
#'
# Function to construct the final list object
# 100, min is 1, max is 2000
make_json <-
  function(fields,
           order_by = "",
           sort_direction = "asc",
           query,
           limit = 100,
           before_id = "",
           after_id = "") {
    # Check if before or after id have been specified
    if (before_id == "" & after_id == "") {
      query <- vector(mode = "list", length = 4)
      names(query) <- c("field_ids", "order", "query", "limit")
    } else if (before_id != "" & after_id != "") {
      # Both are filled out, return an early error
      stop("Specify either before or after id, not both at once.")
    } else {
      # Either before or after id has been filled out, make it 5 elements
      query <- vector(mode = "list", length = 5)
      # Check if it was before or after for naming
      if (before_id == "") {
        # Put after id in name and fill out as element in list
        names(query) <- c("field_ids", "order", "query", "limit", "after_id")
        query[[5]] <- after_id
      } else {
        # Put before id in name and fill out as element in list
        names(query) <- c("field_ids", "order", "query", "limit", "before_id")
        query[[5]] <- before_id
      }
    }
    
    # Fill out the other 4 elements
    query[[1]] <- fields
    query[[2]] <- make_order(order_by, sort_direction)
    query[[3]] <- query
    query[[4]] <- limit
    
    # Return list
    query
  }