% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{georgia_census}
\alias{georgia_census}
\title{Fulton County and Gwinnett County, GA, Census demographic dataset.}
\format{
A nested list which can be sent to the `wru_predict_race_wrapper` function.
Within "GA", the "block", "tract", and "county" keys contain the following columns.
\describe{
 \item{state}{State FIPS code}
 \item{county}{County FIPS code}
 \item{tract}{Tract FIPS code}
 \item{block}{Block FIPS code}
 \item{P005003}{White alone population}
 \item{P005004}{Black or African American alone population}
 \item{P005005}{American Indian and Alaska Native alone population}
 \item{P005006}{Asian alone population}
 \item{P005007}{Native Hawaiian and Other Pacific Islander alone population}
 \item{P005008}{Some other race alone population}
 \item{P005009}{Two or more races population}
 \item{P005010}{Hispanic or Latino population}
 \item{r_whi}{White voters; from Census Bureau.}
 \item{r_bla}{Black voters; from Census Bureau.}
 \item{r_his}{Hispanic voters; from Census Bureau.}
 \item{r_asi}{Asian voters; from Census Bureau.}
 \item{r_oth}{Other voters; from Census Bureau.}
}
}
\source{
Census Bureau via the WRU package.
}
\usage{
data(georgia_census)
}
\description{
This dataset contains the demographic information for Fulton and Gwinnett counties
in Georgia.
}
\keyword{datasets}
