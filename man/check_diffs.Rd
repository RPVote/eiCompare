% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ei_preprocessing.R
\name{check_diffs}
\alias{check_diffs}
\title{Internal function that checks for adequate closeness between sums
of race/candidate columns and provided vote totals.}
\usage{
check_diffs(vote_sums, provided_totals, max_dev = 0.1, avg_dev = 0.25)
}
\arguments{
\item{vote_sums}{A numeric vector containing the sums of votes by each
race/ethnicity}

\item{provided_totals}{A numeric vector containing the totals provided to the
clean_race function}

\item{max_dev}{A floating point object passed from clean_race}

\item{avg_dev}{A floating point object passed from clean_race}
}
\value{
A list containing two objects, 'closeness' and 'deviates'. See
  details for more information.
}
\description{
This function checks conditions inputted to clean_race by the user
}
\details{
When this function returns 0, votes do not sum close enough
to the provided vote totals.

When this function returns 1, votes sum  marginally away
from the provided totals. Restandardization is appropriate

When this function returns 2, votes sum exactly to vote
totals
}
\author{
Ari Decter-Frain
}
\keyword{internal}
