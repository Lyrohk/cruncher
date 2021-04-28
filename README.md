
# cruncher

<!-- badges: start -->
<!-- badges: end -->

The goal of cruncher is to speed up the reasearch process and simplify the work with Crunchbase API.
There are three keywords to keep in mind when using this package:

1. __autocomplete__
2. __search__
3. __lookup__

Together, these three keywords cover the three API endpoints, Autocomplete API, Search Entity API, and Lookup Entity API. In addition, you might want to keey __get__ in mind, as it will come in handy to quickly get available paths, operators, or cards.

## Installation

You can install the released version of cruncher with the use of devtools:

``` r
## Install devtools if you haven't done so already
install.packages("devtools")

## After installation, use it to install cruncher
devtools::install_github("lyrohk/cruncher")

## Load cruncher
library(cruncher)

## Set your academic research access Crunchbase API key 
setAPIKey()
```

## Example

This is a basic example which shows you how to leverage cruncher to retrieve data about AI Startups in Germany:

``` r
## If you haven't already, store your research Crunchbase API key as in environment variable with the help of
setAPIKey()

## Use the Autocomplete API 
auto_output <- autocomplete(within = "categories", search_for = "Artificial Intelligence")

## Use the Search API 
ai_condition <- searchCondition("categories", "includes", auto_output$uuid[1])
age_condition <- searchCondition("founded_on", "gte", 2015)
location_condition <- searchCondition("location_identifiers", "includes", "Germany")
conditions <- rbind(ai_condition, age_condition, location_condition)
search_output <- searchForOrganizations(conditions, uuids_only = T)

## Use the Entity Lookup API
lookup_output <- lookupOrganizations(search_output$uuid)
```

