\name{goodman_generalize}
\alias{goodman_generalize}

\title{
Goodman Regression Generalization
}
\description{
Makes summary table out of multiple heckman regression results, for multiple candidates and groups
}
\usage{
goodman_generalize(cand_vector, race_group, total, data, table_names, ...)
}

\arguments{
  \item{cand_vector}{
Character vector of candidate names, taken from the dataset
}
  \item{race_group}{
Character vector of formula, e.g., "~ pct_latino"
}
  \item{total}{
Character vector (e.g., "totvote") of total variable name from data, variable in data is numeric
}
  \item{data}{
data.frame() object containing the data
}
  \item{table_names}{
Character vector of table names with same length as race_group. Used for formatting output
}
  \item{\dots}{
Arguments passed onto lm() function
}
}

\value{
Object of class data.frame() returned containing table summary of all the Goodman regressions}
\references{
eiPack 
King et. al. (http://gking.harvard.edu/eiR)
L. A. Goodman. Ecological regressions and behavior of individuals. American Sociological Review, 1953. 
}
\author{
Loren Collingwood <loren.collingwood@ucr.edu>
}

\seealso{
\code{\link{ei_rc_good_table}}
}
\examples{
# Load corona data
data(corona)
# Generate character vectors
cands <- c("pct_husted","pct_spiegel","pct_ruth","pct_button","pct_montanez","pct_fox")
race_group3 <- c("~ pct_hisp", "~ pct_asian", "~ pct_white")

# Goodman Regression
table_names <- c("Good: Pct Lat", "Good: Pct Asian", "Good: Pct Wht")
good_corona <- goodman_generalize(cands, race_group3, "totvote", corona, table_names)
}
