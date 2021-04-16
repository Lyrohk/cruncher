#' Helper function to print out the error code along with a short explanation of what it means
#'
#'@param status_code the response code returned from the http request
#'@return a message that is printed to the console
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' printError(404)
#'
#'
printError <- function(response_code) {
  if (response_code == 400) {
    cat("Information:")
    cat(paste("Request returned the error code", response_code))
    cat("One of the following issues are likely the cause:")
    cat(c("Multiple pagination parameters are specified in the api call", 
            "Api call contains non-json body or an invalid json search request",
            "The request uri is invalid",
            "Query timeout is exceeded",
            "The specified operator_id is unknown or invalid",
            "The specified values are invalid",
            "The format isn't supported."))
    cat("Please check and try again.")
  } else if (response_code == 401) {
    cat("Information:")
    cat(paste("The specified API_KEY is invalid and request and returned error code", response_code))
  } else if (response_code == 404) {
    cat("Information:")
    cat(paste("The specified entity collection id is invalid, the resource cannot be found by its name , or the field_id does not exist. The request returned the error code", response_code))
  } else if (response_code == 403) {
    cat("Information:")
    cat(paste("Request is asking for more than 1000 results and returned error code", response_code))
  } else if (response_code == 409) {
    cat("Information:")
    cat(paste("Too many requests are made and you're rate-limited. The status code returned the error ", response_code))
  } else if (response_code == 429) {
    cat("Information:")
    cat(paste("Too many concurrent requests are made and the error code returned", response_code))
  } else if (response_code == 500) {
    cat("Information:")
    cat(paste("A generic error occurred, likely due to an internal problem. The error code is", response_code))
  } else if (response_code == 502) {
    cat("Information:")
    cat(paste("The service is currenctly unavailable due to an outage. The error code is", response_code))
  } else {
    # Unknown status code, return as is
    cat(paste("Error", response_code))
  }
}