% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lookupFundingRoundCard.R
\name{lookupFundingRoundCard}
\alias{lookupFundingRoundCard}
\title{Function to lookup and paginate through a single card for a single funding round over the Entity Lookup API Endpoint}
\usage{
lookupFundingRoundCard(
  funding_round_card,
  funding_round_id,
  please_parse = TRUE
)
}
\arguments{
\item{please_parse}{Logical. By default TRUE and will parse your data from a list of data.frames to a final data.frame with empty elements dropping out.}

\item{entity_card}{card field of interest that will be returned. Only one please!}

\item{entity_id}{UUID or permalink of the funding round you wish to look up}
}
\value{
a data.frame
}
\description{
Takes the entity id to lookup a card and paginate through the entities and parse them into a data.frame
}
\examples{
lookupFundingRoundCard(funding_round_card = "degrees", funding_round_id = "mark-zuckerberg")

}
\author{
Layla Rohkohl, \email{byehity@gmail.com}
}
