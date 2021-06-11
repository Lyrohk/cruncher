#' Function to lookup and paginate through a single card for a single entity over the Entity Lookup API Endpoint
#'
#' Takes the entity id to lookup a card and paginate through the entities and parse them into a data.frame
#'
#'@param entity_card card field of interest that will be returned. Only one please!
#'@param entity_id UUID or permalink of the organization you wish to look up
#'@param entity_path Type of entity e.g. organizations
#'@param please_parse to pretty parse the final data.frame e.g. removing nestled lists
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
lookupEntityCard <- function(entity_card, entity_id, entity_path, please_parse = TRUE) {

  # Check API_KEY
  API_KEY <- Sys.getenv("API_KEY")

  # Check that API_KEY exists
  if (API_KEY == "") {
    stop("Please set a valid user key with academic research access to the Crunchbase API with setAPIKey().")
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

  # Create the entity_path
  url <- paste0("https://api.crunchbase.com/api/v4/entities/", entity_path, "/", entity_id, "/cards/", entity_card, "?user_key=", API_KEY)

  # Make http GET request
  response <- RETRY(verb = "GET", url = url, quiet = T) # Could use GET but someone may apply it with a list

  # Check if we get valentity_id data, if not return error core
  if (response$status_code == 200) {
    # Get data
    data <- fromJSON(response$url)

    # Initialize final df
    list_of_df <- list()
    page_no <- 1


    # Put into list the first df
    first_df <- data$cards[[entity_card]]
    list_of_df$page_1 <- first_df

    # Return an empty data.frame if the length is 0
    if (length(data$cards[[entity_card]]) == 0) {
      return(data.frame())
    }

    # Check
    check <- NROW(first_df)

    # Paginate while check is 100
    while (check == 100) {
      # Get last uuid of list_of_df
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
      data <- fromJSON(next_res$url)

      # Get next df
      next_df <- data$cards[[entity_card]]

      # Append to original
      list_of_df[[paste0("page_", page_no)]] <- next_df

      # Construct new check
      check <- NROW(next_df)
    }

    # Get final data.frame
    final_df <- rbind_pages(list_of_df)
    # Check please_parse
    if (please_parse) {
      # Split each row of the data.frame into a list and pass this as data to be parsed to the parsing function
      data_list <- split(final_df, seq(nrow(final_df)))

      # Get right parser
      parsingFun <- getParserForCard(entity_card)

      # Apply to each element of the list and pack back together into final parsed data.frame
      df <- do.call(rbind.data.frame, lapply(data_list, parsingFun))

      # Filter out NA columns
      return(df[colSums(!is.na(df)) > 0])

    } else {
      # Return the final data.frame which may contain nestled lists
      return(final_df)
    }

  } else {
    # Print out error with message
    printError(response$status_code == 200)
  }
}