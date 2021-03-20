
# cruncher

<!-- badges: start -->
<!-- badges: end -->

The goal of cruncher is to speed up the reasearch process and simplify the work with Crunchbase API.

## Installation

You can install the released version of cruncher from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("cruncher")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(cruncher)
## basic example code
API_KEY <- "your_research_api_key"

## Use the autocomplete api endpoint
output <- autocomplete("categories", query = "AI", limit = 10)
```

