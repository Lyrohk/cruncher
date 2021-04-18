#' Helper function to convert funding stage to human readable numbers
#'
#'@return Human understandable format of the funding stage e.g. "M&A"
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' convertFundingStage("early_stage_venture")
#'
#'
# stages Converter Function
convertFundingStage <- function(stage) {
  # Check if missing
  if (stage == "" | is.na(stage)) {
    return(NA)
  } else {
    # Convert the stages if they are recognized
    if (stage == "early_stage_venture") {
      return("Early Stage Venture")
    } else if (stage == "ipo") {
      return("IPO")
    } else if (stage == "late_stage_venture") {
      return("Late Stage Venture")
    } else if (stage == "m_and_a") {
      return("M&A")
    } else if (stage == "private_equity") {
      return("Private Equity")
    } else if (stage == "seed") {
      return("Seed")
    } else {
      # Keep it as is
      return(stage)
    }
  }
}