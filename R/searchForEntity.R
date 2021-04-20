#' Function to search for some entities matching certain search criteria over the Search API Endpoint
#'
#'@param path The path to the entity e.g. organizations, people, etc.
#'@param search_conditions of what you are looking for
#'@param please_parse TRUE or FALSE. By default TRUE. If set to FALSE, it will return the data directly from the JSON, if set to TRUE, it will parse it into a data.frame object
#'@return either a data.frame (if parse = TRUE) or a list (if parse = FALSE)
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' lookupEntity("facebook", "organizations", please_parse = T)
#'
#' @import httr
#' @import jsonlite
#'
#'
searchForEntity <- function(conditions, order_by, sort_by, path, no_results) {

  # Quick to failure section ####
  if (!sort_by %in% c("asc", "desc")) {
    stop("Sort direction needs to be either 'asc' or 'desc'.")
  }


  # Functionality section ####
  field_ids <- "identifier"
  order <- data.frame("field_id" = order_by,
                      "sort" = sort_by)
  query <- conditions
  limit <- 2000 #Max; min 1; default 100
  after_id <- "blub"
}