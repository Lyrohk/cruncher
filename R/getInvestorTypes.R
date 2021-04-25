#' Helper function to return the investor types dictionary
#'
#'@return dictionary
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' getInvestorTypes()
#'
#' @export
#'
getInvestorTypes <- function() {
  return(c(
    "accelerator" = "Accelerator",
    "angel" = "Individual/Angel",
    "angel_group" = "Angel Group",
    "co_working_space" = "Co-Working Space",
    "corporate_venture_capital" = "Corporate Venture Capital",
    "entrepreneurship_program" = "Entrepreneurship Program",
    "family_investment_office" = "Family Investment Office",
    "fund_of_funds" = "Fund Of Funds",
    "government_office" = "Government Office",
    "hedge_fund" = "Hedge Fund",
    "incubator" = "Incubator",
    "investment_bank" = "Investment Bank",
    "investment_partner" = "Investment Partner",
    "micro_vc" = "Micro VC",
    "pension_funds" = "Pension Funds",
    "private_equity_firm" = "Private Equity Firm",
    "secondary_purchaser" = "Secondary Purchaser",
    "startup_competition" = "Startup Competition",
    "syndicate" = "Syndicate",
    "university_program" = "University Program",
    "venture_capital" = "Venture Capital",
    "venture_debt" = "Venture Debt"
  ))
}

