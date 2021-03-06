% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parseDegree.R
\name{parseDegree}
\alias{parseDegree}
\title{Function to parse Degree Entity from JSON list to Data.frame}
\usage{
parseDegree(data)
}
\arguments{
\item{data}{Returned fields card response in json list form for the entity}
}
\value{
a data.frame of parsed output
}
\description{
Function to parse Degree Entity from JSON list to Data.frame
}
\examples{
parseDegree(data$cards$fields)

}
\author{
Layla Rohkohl, \email{byehity@gmail.com}
}
