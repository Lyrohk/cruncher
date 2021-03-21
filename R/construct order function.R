# Hidden function to make order
make_order <- function(order_by, sort_direction) {
  # Construct list
  inner_list <- vector(mode="list", length=2)
  names(inner_list) <- c("field_id", "sort")
  inner_list[[1]] <- order_by
  inner_list[[2]] <- sort_direction
  
  # Return list
  order <- vector(mode="list", length=1)
  order[[1]] <- inner_list
  order
}