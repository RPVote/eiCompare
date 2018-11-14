\name{precinct_agg_combine}
\alias{precinct_agg_combine}
\title{precinct_agg_combine}
\description{
Combines voter file surname geocoded data into aggregated precinct counts. To be performed on data object resulting from successful completion of bisg_prep_race_predict() function.
}
\usage{
precinct_agg_combine(dat, precinct = "precinct")
}
\arguments{
  \item{dat}{data.frame() object}
  \item{precinct}{Character of precinct column name. Default = "precinct", which is what   is output from bisg_prep_race_predict()}
}
\value{
Aggregated dataset of nrow() precinct size, including racial size precinct estimates. Dataset suitable for EI/RxC.
}
\author{
Loren Collingwood <loren.collingwood@ucr.edu>
}

\examples{
}
