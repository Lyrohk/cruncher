% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/autocomplete function.R
\name{autocomplete}
\alias{autocomplete}
\title{Function to use the Autocomplete API endpoint of Crunchbase API}
\usage{
autocomplete(search_for, within = "", limit = 10)
}
\arguments{
\item{search_for}{search_for we are looking for e.g. "Europe" in locations, "Artificial Intelligence" in categories}

\item{within}{A comma separated list of collection ids to search against e.g. "categories,category_groups". Defaults to all.}

\item{limit}{Number of entries to be returned at most (min 1, default 10, max 25)}
}
\value{
the entities matching the search_for within the specified within and with the limit specified
}
\description{
Function to use the Autocomplete API endpoint of Crunchbase API
}
\examples{
autocomplete(search_for = "Artificial Intelligence", within = "categories", limit = 5)

}
\author{
Layla Rohkohl, \email{byehity@gmail.com}
}
