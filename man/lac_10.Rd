\name{lac_10}
\alias{lac_10}
\docType{data}
\title{
Los Angeles County Data}
\description{
Los Angeles County precinct dataset from 2010 election
}
\usage{data("lac_10")}
\format{
  A data frame with 4980 observations on the following 10 variables.
  \describe{
    \item{\code{precinct}}{a character vector}
    \item{\code{tot_reg}}{a numeric vector}
    \item{\code{i_jones}}{a numeric vector}
    \item{\code{i_delatore}}{a numeric vector}
    \item{\code{votescast}}{a numeric vector}
    \item{\code{lat_voters}}{a numeric vector}
    \item{\code{pct_latino}}{a numeric vector}
    \item{\code{pct_delatorre}}{a numeric vector}
    \item{\code{pct_jones}}{a numeric vector}
    \item{\code{pct_other}}{a numeric vector}
  }
}
\source{
Los Angeles county 
}
\examples{
data(lac_10)
head(lac_10)
str(lac_10)
}
\keyword{datasets}
