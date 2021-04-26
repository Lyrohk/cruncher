#' Function to search for some entities matching certain search criteria over the Search API Endpoint
#'
#'@param path The path to the entity e.g. organizations, people, etc.
#'@param search_conditions of what you are looking for
#'@param please_parse TRUE or FALSE. By default TRUE. If set to FALSE, it will return the data directly from the JSON, if set to TRUE, it will parse it into a data.frame object
#'@param personal_limit Personal set limit of result. Useful when only needing e.g. the top 50 organizations in a certain category.
#'@param iter Number of results to be returned per call. Default here 1000L, max 2000L. Usually defaulting to 50.
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
searchForEntity <- function(path,
                            conditions,
                            order_by = "identifier",
                            sort_by = "asc",
                            personal_limit = NA,
                            iter = 1000L,
                            uuids_only = FALSE) {

  # TODO API key check

  # Check that path in getPaths()
  if (!path %in% getPaths()) {
    stop("Path in searchforentity must be a valid one. Call getPaths() to view them.")
  }

  # Check that search_conditions exists and is a data.frame with at least one row
  if (missing(conditions)) {
    stop("The argument search_conditions needs to be specified. Use rbind with the created searchCondition() function.")
  }
  if (class(conditions) != "data.frame") {
    stop("The argument search_conditions must be of type 'data.frame'. Use rbind with the searchCondition or searchConditionCurrency functions.")
  }

  # Quick to failure section ####
  if (!sort_by %in% c("asc", "desc")) {
    stop("Sort direction needs to be either 'asc' or 'desc'.")
  }

  # Make JSON body
  json <- makeJson(order_by = order_by,
                    sort_direction = sort_by,
                    query = conditions,
                    limit = iter)

  # Print json
  print(json)

  # Make POST request
  response <- RETRY(verb = "POST",
                    url = paste0("https://api.crunchbase.com/api/v4/searches/", path, "?user_key=", API_KEY),
                    body = toJSON(json, flatten = T, auto_unbox = T))

  # Check if we get valid data, if not return error core
  if (response$status_code == 200) {

    # Get initial data and the number of total entities
    data <- fromJSON(rawToChar(response$content))

    # Get total count of entities
    num_total_entities <- data$count

    # Print out how many counts we have
    cat(paste("The Search Result has", num_total_entities, "results. Please wait while the results are being fetched.\n"))

    # Get entity uuids
    entity_uuids <- data.frame("uuid" = data[["entities"]][["uuid"]])

    # Get number of entities returned
    num_returned_entities <- NROW(entity_uuids)
    cat(paste("The number of returned uuids currently amounts to",  num_returned_entities, "\n"))

    # Check if num_total_entities more than returned ones
    while ((num_total_entities - num_returned_entities) > iter) {
      # Put last uuid as after_id parameter to the body
      json$after_id <- tail(entity_uuids$uuid, 1)

      # Print last uuid
      print(paste("Last uuid is", json$after_id))

      # Request the next batch of uuids
      response <- RETRY(verb = "POST",
                        url = paste0("https://api.crunchbase.com/api/v4/searches/", path, "?user_key=", API_KEY),
                        body = toJSON(json, flatten = T, auto_unbox = T))

      # Get next data
      data <- fromJSON(rawToChar(response$content))

      # Get next uuids
      next_uuids <- data.frame("uuid" = data[["entities"]][["uuid"]])

      # Combine next batch to current batch
      entity_uuids <- rbind(entity_uuids, next_uuids)

      # Update the current number of returned entities
      num_returned_entities <- NROW(entity_uuids)
      cat(paste("The number of returned uuids currently amounts to",  num_returned_entities, "\n"))
      }

    # Return wanted output
    if (uuids_only) {
      # Return uuids as is
      return(entity_uuids)
    } else {
      # Inform user that uuids are retrived and now information is being looked up
      cat(paste("Cruncher has retrieved all entity uuids. Now information is going to be fetched for each uuid via the Entity Lookup API.\n"))

      # Get information via Entity Lookup API Endpoint
      return(lookupEntities(entities = entity_uuids$uuid, path = path, please_parse = T))
    }
  } else {
    # Print error code
    printError(response$status_code)
  }
}