
# cruncher

<!-- badges: start -->
<!-- badges: end -->

The goal of cruncher is to speed up the reasearch process and simplify the work with Crunchbase API.

## Installation

You can install the released version of cruncher with the use of devtools:

``` r
## Install devtools if you haven't done so already
install.packages("devtools")

## After installation, use it to install cruncher
devtools::install_github("lyrohk/cruncher")

## Load cruncher
library(cruncher)
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
## Store your research Crunchbase API key as in environment variable
API_KEY <- "your_research_api_key"

## Use the autocomplete api endpoint
output <- autocomplete(collection_ids = "categories", query = "AI", limit = 10)
```

