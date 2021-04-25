#' Helper function to return all cards for one of the paths
#'
#'@return all valid cards for a particular path
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @param path of interest
#' @param pretty_print TRUE by default, then it will print out an explanatory line before the list of available cards for a path
#'
#' @examples
#' getCardsFor("organizations")
#' getCardsFor("funding_rounds")
#'
#'@import stringr
#'@import dplyr
#' @export
#'
# Helper function to view all paths possible
getCardsForPath <- function(path, pretty_print = TRUE) {

  # String manipulations
  path <- path %>%
    str_to_lower() %>%
    str_trim() %>%
    str_replace_all(" ", "_")

  # Cards
  organization_cards <- c(
    "acquiree_acquisitions",
    "acquirer_acquisitions",
    "child_organizations",
    "child_ownerships",
    "event_appearances",
    "founders",
    "headquarters_address",
    "investors",
    "ipos",
    "jobs",
    "parent_organization",
    "parent_ownership",
    "participated_funding_rounds",
    "participated_funds",
    "participated_investments",
    "press_references",
    "raised_funding_rounds",
    "raised_funds",
    "raised_investments"
  )
  people_cards <-
    c(
      "degrees",
      "event_appearances",
      "founded_organizations",
      "jobs",
      "participated_funding_rounds",
      "participated_funds",
      "participated_investments",
      "partner_funding_rounds",
      "partner_investments",
      "press_references",
      "primary_job",
      "primary_organization"
    )
  funding_round_card <- c(
    "investments" ,
    "investors" ,
    "lead_investors" ,
    "organization" ,
    "partners" ,
    "press_references"
  )
  acquisition_cards <- c("acquiree_organization",
                         "acquirer_organization",
                         "press_references")
  investment_cards <- c("funding_round" ,
                        "investor" ,
                        "organization",
                        "partner")
  event_cards <- c(
    "address",
    "appearances",
    "contestants",
    "exhibitors",
    "organizers",
    "press_references",
    "speakers",
    "sponsors"
  )
  fund_cards <- c("investors", "owner", "press_references")
  event_appearance_card <- c("event", "participant")
  ipo_card <- c("organization", "press_references")
  ownership_cards <-
    c("child_organization",
      "parent_organization",
      "press_references")
  job_cards <- c("organization", "person")
  address_cards <- c("event", "organization")
  degree_cards <- c("organization", "person")

  # Check pretty print parameter
  if (pretty_print) {
    # Return cat and lists
    # Depending on path, print out the right card to console
    if (path == "organizations") {
      cat(paste0("Here are the cards available for ", path, ":\n"))
      return(organization_cards)
    } else if (path == "people") {
      cat(paste0("Here are the cards available for ", path, ":\n"))
      return(people_cards)
    } else if (path == "acquisitions") {
      cat(paste0("Here are the cards available for ", path, ":\n"))
      return(acquisition_cards)
    } else if (path == "investments") {
      cat(paste0("Here are the cards available for ", path, ":\n"))
      return(investment_cards)
    } else if (path == "events") {
      cat(paste0("Here are the cards available for ", path, ":\n"))
      return(event_cards)
    } else if (path == "event_appearances") {
      cat(paste0("Here are the cards available for ", path, ":\n"))
      return(event_appearance_card)
    } else if (path == "funds") {
      cat(paste0("Here are the cards available for ", path, ":\n"))
      return(fund_cards)
    } else if (path == "funding_rounds") {
      cat(paste0("Here are the cards available for ", path, ":\n"))
      return(funding_round_card)
    } else if (path == "ipos") {
      cat(paste0("Here are the cards available for ", path, ":\n"))
      return(ipo_card)
    } else if (path == "jobs") {
      cat(paste0("Here are the cards available for ", path, ":\n"))
      return(job_cards)
    } else if (path == "degrees") {
      cat(paste0("Here are the cards available for ", path, ":\n"))
      return(degree_cards)
    } else if (path == "ownerships") {
      cat(paste0("Here are the cards available for ", path, ":\n"))
      return(ownership_cards)
    } else if (path == "addresses") {
      cat(paste0("Here are the cards available for ", path, ":\n"))
      return(address_cards)
    } else if (path %in% c("categories", "category_groups", "locations", "press_references")) {
      cat("Categories, category groups, press references, and locations all only have the basic properties field already returned by default.")
    } else {
      # Path was not recognized, stop here
      stop("Path was not recognized.\n You can check the right spelling of all paths available by calling getPaths().")
    }
  } else {
    # Just return the lists
    # Depending on path, print out the right card to console
    if (path == "organizations") {
      return(organization_cards)
    } else if (path == "people") {
      return(people_cards)
    } else if (path == "acquisitions") {
      return(acquisition_cards)
    } else if (path == "investments") {
      return(investment_cards)
    } else if (path == "events") {
      return(event_cards)
    } else if (path == "event_appearances") {
      return(event_appearance_card)
    } else if (path == "funds") {
      return(fund_cards)
    } else if (path == "funding_rounds") {
      return(funding_round_card)
    } else if (path == "ipos") {
      return(ipo_card)
    } else if (path == "jobs") {
      return(job_cards)
    } else if (path == "degrees") {
      return(degree_cards)
    } else if (path == "ownerships") {
      return(ownership_cards)
    } else if (path == "addresses") {
      return(address_cards)
    } else if (path %in% c("categories", "category_groups", "locations", "press_references")) {
      cat("Categories, category groups, press references, and locations all only have the basic properties field already returned by default.")
    } else {
      # Path was not recognized, stop here
      stop("Path was not recognized.\n You can check the right spelling of all paths available by calling getPaths().")
    }
  }
}