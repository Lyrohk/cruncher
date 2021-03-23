# Function to construct the final list object
# 100, min is 1, max is 2000
make_query <-
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
    query[[2]] <- order(order_by, sort_direction)
    query[[3]] <- query
    query[[4]] <- limit
    
    # Return list
    query
  }