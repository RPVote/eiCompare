\name{ei_rc_congruence}
\alias{ei_rc_congruence}
\title{Congruence for 2x2}
\description{Calculates congruence scores between EI and RxC for the 2x2 Scenario}
\usage{ei_rc_congruence(ei_rc_table, cand_race, group_race)}
\arguments{
  \item{ei_rc_table}{Object produced from ei_rc_good_table(), where include_good=F, of class ei_compare}
  \item{cand_race}{Numeric vector indicating race of the candidates in order they show up in table rownames,
             where 1=Latino; 2=Black; 3=Asian; 4=White/Non}
  \item{group_race}{Numeric vector, taking similar values as cand_race where 1=Latino; 2=Black; 3=Asian; 4=White/Non}
}
\value{
Table of congruence scores
}
%\references{
%eiPack, King et. al. (http://gking.harvard.edu/eiR)
%}
\author{
Loren Collingwood <loren.collingwood@ucr.edu>, Matt Barreto <barretom@ucla.edu>
}

\examples{

# LA County 2010 Insurance Commissioner Race
#ei_rc_combine <- ei_rc_good_table(results, ei_bayes_res, 
#                                  groups= c("Latino", "Non Latino"))

load ( system.file("extdata/congruence_table.RData",package="eiCompare") )

ei_rc_congruence(ei_rc_combine2_2, c(1,4), c(1,4)) 

}