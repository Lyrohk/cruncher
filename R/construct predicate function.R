#' Function to make predicate as part of the query JSON request body to the Search API endpoint of Crunchbase API
#'
#'@param collection_ids Within which path the matching to the query should be performed e.g. organizations
#'@param query What we are looking for e.g. "Europe" in locations, "Artificial Intelligence" in categories
#'@param limit Number of entries to be returned at most (min 1, default 10, max 25)
#'@return the entities matching the query within the specified collection_ids and with the limit specified
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' predicate_1 <- make_predicate(field = "location_identifiers", operator = "includes", value = "6106f5dc-823e-5da8-40d7-51612c0b2c4e") in europe
#' predicate_2 <- make_predicate(field = "funding_total", operator = "between", value = "{"value": 25000000,"currency": "usd"},{"value": 100000000,"currency":"usd"}")
#' predicate_3 <- make_predicate(field = "facet_ids", operator = "includes", value = "company")
#' predicate_4 <- make_predicate(field = "num_employees_enum", operator = "includes", value = "c_00101_00250") between 101 and 250 employees
#' predicate_5 <- make_predicate(field = "categories", operator = "includes", value = "58842728-7ab9-5bd1-bb67-e8e55f6520a0") for biotech
#'
#' @export
#'
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