\name{corona}
\alias{corona}
\docType{data}
\title{
Corona Precinct Racial Bloc Voting Data
}
\description{
Data taken from a 2014 California election, with precinct results and racial demographics for Corona, CA precincts
}
\usage{data("corona")}
\format{
  A data frame with 46 observations on the following 12 variables.
  \describe{
    \item{\code{precinct}}{a numeric vector}
    \item{\code{totvote}}{a numeric vector}
    \item{\code{pct_husted}}{a numeric vector}
    \item{\code{pct_spiegel}}{a numeric vector}
    \item{\code{pct_ruth}}{a numeric vector}
    \item{\code{pct_button}}{a numeric vector}
    \item{\code{pct_montanez}}{a numeric vector}
    \item{\code{pct_fox}}{a numeric vector}
    \item{\code{pct_hisp}}{a numeric vector}
    \item{\code{pct_asian}}{a numeric vector}
    \item{\code{pct_white}}{a numeric vector}
    \item{\code{pct_non_lat}}{a numeric vector}
  }
}

\references{
Riverside County, CA board of elections
}
\examples{
data(corona)
head(corona)
str(corona)
}
\keyword{datasets}
