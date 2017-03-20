\name{cor_06}
\alias{cor_06}
\docType{data}
\title{
Corona 2006 
}
\description{
Precinct vote data from a Corona, CA 2006 election
}
\usage{data("cor_06")}
\format{
  A data frame with 47 observations on the following 8 variables.
  \describe{
    \item{\code{precinct}}{a numeric vector}
    \item{\code{totvote}}{a numeric vector}
    \item{\code{pct_latino}}{a numeric vector}
    \item{\code{pct_other}}{a numeric vector}
    \item{\code{pct_breitenbucher}}{a numeric vector}
    \item{\code{pct_montanez}}{a numeric vector}
    \item{\code{pct_spiegel}}{a numeric vector}
    \item{\code{pct_skipworth}}{a numeric vector}
  }
}

\references{
Riverside County, CA board of elections
}
\examples{
data(cor_06)
# Look at data
head(cor_06)
str(cor_06)
}
\keyword{datasets}
