#' Silence Function
#'
#' Helper function to silence a function
#'
#'@param x some function
#'@return nothing
#'
#'
#'
#'
silenceFun <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}