#' Helper function to return the funding stages dictionary
#'
#'@return dictionary
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' getFundingStages()
#'
#' @export
#'
getFundingStages <- function() {
  return(c(
    "early_stage_venture" = "Early Stage Venture",
    "ipo" = "IPO",
    "late_stage_venture" = "Late Stage Venture",
    "m_and_a" = "M&A",
    "private_equity" = "Private Equity",
    "seed" = "Seed"
  ))
}