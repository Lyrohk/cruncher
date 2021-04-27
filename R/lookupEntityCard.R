#' Function to lookup and paginate through a single card for a single entity over the Entity Lookup API Endpoint
#'
#' Takes the entity id to lookup a card and paginate through the entities and parse them into a data.frame
#'
#'@param entity_card card field of interest that will be returned. Only one please!
#'@param entity_id UUID or permalink of the organization you wish to look up
#'@param entity_path Type of entity e.g. organizations
#'@return a list of paginated data.frames
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' lookupEntityCard(entity_card = "investors", entity_id = "facebook", entity_path = "organizations")
#'
#' @import httr
#' @import jsonlite
#' @import stringr
#' @import dplyr
#'
lookupEntityCard <- function(entity_card, entity_id, entity_path) {

  # Check that API_KEY exists
  if (!exists("API_KEY")) {
    stop("Please set a valentity_id user key to API_KEY as an environmental variable or for use setAPIKey() to do it for you.")
  }

  # Check that all arguments exists
  if (missing(entity_id)) {
    stop("An entity uuid or permalink needs to be specified in the (hidden) lookupEntityCard function.")
  }
  if (missing(entity_path)) {
    stop("A path needs to be specified in the (hidden) lookupEntityCard function.")
  }
  if (missing(entity_card)) {
    stop("A card needs to be specified in the (hidden) lookupEntityCard function.")
  }

  # Check that path is part of the available ones
  if (!entity_path %in% getPaths(pretty_print = F)) {
    stop("Path needs to be one of the available ones. Call getPaths() to check 'em out.")
  }

  # Check that card is within the path's card
  if (!entity_card %in% getCardsForPath(entity_path, pretty_print = F)) {
    stop("Card needs to one of the path's card. Call getCardsForPath('yourPath') to check the available ones.")
  }

  # If card is 'fields' use lookupEntity function instead
  if (entity_card == "fields") {
    return(lookupEntity(id = entity_id, path = entity_path))
  }

  # Entity entity_id check e.g. to lower, trimming whitespace, and replacing spaces with -
  entity_id <- entityIdCheck(entity_id)

  # Get right parser
  #parsingFun <- getParserForCard(entity_card)

  # Create the entity_path
  url <- paste0("https://api.crunchbase.com/api/v4/entities/", entity_path, "/", entity_id, "/cards/", entity_card, "?user_key=", API_KEY)

  # Make http GET request
  response <- RETRY(verb = "GET", url = url) # Could use GET but someone may apply it with a list

  # Check if we get valentity_id data, if not return error core
  if (response$status_code == 200) {
    # Get data
    data <- fromJSON(rawToChar(response$content))

    # Initialize final df
    final_df <- list()
    page_no <- 1


    # Put into list the first df
    first_df <- data$cards[[entity_card]]
    final_df$page_1 <- first_df

    # Check NROW
    check <- NROW(first_df)

    # Paginate while check is 100
    while (check == 100) {
      # Get last uuid of final_df
      last_id <- data$cards[[entity_card]]$identifier$uuid[100]

      # Add one to page_no
      page_no <- page_no + 1

      # Print
      cat(paste("Moving on to page", page_no, "...\n"))

      # Make new request
      request_url <- paste0("https://api.crunchbase.com/api/v4/entities/", entity_path, "/", entity_id, "/cards/", entity_card, "?after_id=", last_id, "&user_key=", API_KEY)

      # Get http request
      next_res <- RETRY(verb = "GET", url = request_url)

      # Transform into readable format
      data <- fromJSON(rawToChar(next_res$content))

      # Get next df
      next_df <- data$cards[[entity_card]]

      # Append to original
      final_df[[paste0("page_", page_no)]] <- next_df

      # Construct new check
      check <- NROW(next_df)
    }

    # Return final data.frame
    return(final_df)

  } else {
    # Print out error with message
    printError(response$status_code == 200)
  }
}