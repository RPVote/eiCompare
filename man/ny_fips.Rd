\name{ny_fips}
\alias{ny_fips}
\docType{data}
\title{
New York state FIPS codes}
\description{
New York state FIPS codes example of 500 voters
}
\usage{data("ny_fips")}
\format{
  A data frame with 500 observations on the following 2 variables.
  \describe{
    \item{\code{row_id}}{unique id}
    \item{\code{FIP}}{15 digit FIPS code including state, county, tract, block}
  }
}
\source{FCC}
\examples{
data(ny_fips)
str(ny_fips)
}
\keyword{datasets}
