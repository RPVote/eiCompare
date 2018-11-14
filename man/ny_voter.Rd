\name{ny_voter}
\alias{ny_voter}
\docType{data}
\title{New York voter file sample}
\description{New York voter file sample; example of 500 voters}
\usage{data("ny_voter")}
\format{
  A data frame with 500 observations on the following 10 variables.
  \describe{
    \item{\code{Voter.ID}}{Unique voter file id number, jumbled}
    \item{\code{SD..Poll}}{Precinct id}
    \item{\code{fips}}{15-digit fips code}
    \item{\code{st}}{state fips code}
    \item{\code{county}}{county fips code}
    \item{\code{tract}}{tract fips code}
    \item{\code{block}}{block fips code}
    \item{\code{st_cty}}{state county fips code}
    \item{\code{st_cty_tract}}{state county tract fips code}
    \item{\code{Last.Name}}{Voters' surname}
  }
}
\source{ERCSD board of election}
\examples{
data(ny_voter)
str(ny_voter)
}
\keyword{datasets}
