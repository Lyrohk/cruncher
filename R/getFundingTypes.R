#' Helper function to return funding types dictionary
#'
#'@return a dictionary
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' getFundingTypes()
#'
#' @export
#'
# Helper function to get funding type dictionary
getFundingTypes <- function() {
  return(c("angel"= "Angel",
           "convertible_note" = "Convertible Note",
           "corporate_round" = "Corporate Round",
           "debt_financing" = "Debt Financing",
           "equity_crowdfunding" = "Equity Crowdfunding",
           "grant" = "Grant",
           "initial_coin_offering" = "Initial Coin Offering",
           "non_equity_assistance" = "Non-equity Assistance",
           "post_ipo_debt" = "Post-IPO Debt",
           "post_ipo_equity" = "Post-IPO Equity",
           "post_ipo_secondary" = "Post-IPO Secondary",
           "pre_seed" = "Pre-Seed",
           "private_equity" = "Private Equity",
           "product_crowdfunding" = "Product Crowdfunding",
           "secondary_market" = "Secondary Market",
           "seed" = "Seed",
           "series_a" = "Series A",
           "series_b" = "Series B",
           "series_c" = "Series C",
           "series_d" = "Series D",
           "series_e" = "Series E",
           "series_f" = "Series F",
           "series_g" = "Series G",
           "series_h" = "Series H",
           "series_i" = "Series I",
           "series_j" = "Series J",
           "series_unknown" = "Series Unknown",
           "undisclosed" = "Undisclosed"))
}
