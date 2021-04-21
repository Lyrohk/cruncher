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
searchForEntity <- function(path, conditions, order_by, sort_by, result_limit, uuids_only) {

  # Check that search_conditions exists and is a data.frame with at least one row
  if (missing(conditions)) {
    stop("The argument search_conditions needs to be specified. Use rbind with the created searchCondition() function.")
  }
  if (class(conditions) != "data.frame") {
    stop("The argument search_conditions must be of type 'data.frame'.")
  }

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
  #after_id <- "blub"

  # Construct JSON body as list
  json_list <- list(field_ids, order, query, limit)
  names(json_list) <- c("field_ids", "order", "query", "limit")

  # Make POST request
  response <- RETRY(verb = "POST",
                    url = paste0("https://api.crunchbase.com/api/v4/searches/", path, "?user_key=", API_KEY),
                    body = toJSON(json_list),
                    encode = "json")



  # Parse it into readable content
  data <- fromJSON(rawToChar(response$content))

  # Get total count of entities
  total_entities <- data$count

  # TODO Paginate through if no result limit or result limit above returned number of results

  # Get entity uuids
  entity_uuids <- data[["entities"]][["uuid"]]

  # Return wanted output
  if (uuids_only) {
    return(entity_uuids)
  } else {
    return(lookupEntities(entities = entity_uuids, path = path, please_parse = T))
  }
}