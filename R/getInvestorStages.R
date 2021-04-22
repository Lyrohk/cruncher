#' Helper function to return the investor stages dictionary
#'
#'@return dictionary
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' getInvestorStages()
#'
#' @export
#'
getInvestorStages <- function() {
  return(c(
    "convertible_note" = "Convertible Note",
    "crowdfunding" = "Crowdfunding",
    "debt" = "Debt",
    "early_stage_venture" = "Early Stage Venture",
    "grant" = "Grant",
    "late_stage_venture" = "Late Stage Venture",
    "initial_coin_offering" = "Initial Coin Offering",
    "non_equity_assistance" = "Non-equity Assistance",
    "post_ipo" = "Post-Ipo",
    "private_equity" = "Private Equity",
    "secondary_market" = "Secondary Market",
    "seed" = "Seed",
    "venture " = "Venture"
  ))
}

