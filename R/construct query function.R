# Function to construct the final list object
make_query <- function(fields, order_by, sort_direction, query, limit) {
  # Construct list
  query <- vector(mode="list", length=4)
  names(query) <- c("field_ids", "order", "query", "limit")
  query[[1]] <- fields
  query[[2]] <- order(order_by, sort_direction)
  query[[3]] <- query
  query[[4]] <- limit
  
  # Return list
  query 
}