# Function to make predicate in query
make_predicate <- function(field, operator, value) {
  # Construct list
  predicate <- vector(mode = "list", length = 4)
  names(predicate) <- c("type", "field_id", "operator_id", "values")
  predicate[[1]] <- "predicate"
  predicate[[2]] <- field
  predicate[[3]] <- operator
  predicate[[4]] <- value
  
  # Return list
  predicate
}