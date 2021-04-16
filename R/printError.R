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
    cat(c("Information:", "\n",
          paste("Request returned the error code", response_code), "\n",
          "One of the following issues are likely the cause:", "\n",
          "Multiple pagination parameters are specified in the api call\n", 
            "Api call contains non-json body or an invalid json search request\n",
            "The request uri is invalid\n",
            "Query timeout is exceeded\n",
            "The specified operator_id is unknown or invalid\n",
            "The specified values are invalid\n",
            "The format isn't supported\n", 
          "Please check and try again."))
  } else if (response_code == 401) {
    cat("Information:\n", paste0("The specified API_KEY is invalid and the request returned error code ", response_code, "."))
  } else if (response_code == 404) {
    cat("Information:\n", paste0("The specified entity collection id is invalid, the resource cannot be found by its name, or the field_id does not exist. The request returned the error code ", response_code, "."))
  } else if (response_code == 403) {
    cat("Information:\n", paste0("Request is asking for more than 1000 results and returned error code ", response_code, "."))
  } else if (response_code == 409) {
    cat("Information:\n", paste0("Too many requests are made and you're rate-limited. The status code returned the error ", response_code, "."))
  } else if (response_code == 429) {
    cat("Information:\n", paste0("Too many concurrent requests are made and the error code returned ", response_code, "."))
  } else if (response_code == 500) {
    cat("Information:\n", paste0("A generic error occurred, likely due to an internal problem. The error code is ", response_code, "."))
  } else if (response_code == 502) {
    cat("Information:\n", paste0("The service is currenctly unavailable due to an outage. The error code is ", response_code, "."))
  } else {
    # Unknown status code, return as is
    cat(paste("Error", response_code))
  }
}