#' Function to set API_KEY
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' setAPIKey()
#'
#' @export
#'
# Set API_KEY
setAPIKey <- function(input = ""){

  # Check if input is empty, promt user to enter it
  if (input == "") {
    input <- readline(prompt="Enter your Crunchbase API User Key with research access: ")
  }

  # Set key to system environment
  Sys.setenv(API_KEY = input) # this is a more simple way of storing API keys, it saves it in the .Rprofile file, however this is only temporary - meaning next session the login details will have to be provided again. See below how to store login details in a more durable way.

}
