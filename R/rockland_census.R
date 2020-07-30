#' Rockland County, NY, Census demographic dataset.
#'
#' This dataset contains the demographic information for Rockland County in New
#' York, which is where East Ramapo School District is located.
#'
#' @format A nested list which can be sent to the `predict_race` function in
#'  WRU. Within "NY", the "block", "tract", and "county" keys contain the
#'  following columns.
#' \describe{
#'   \item{state}{State FIPS code}
#'   \item{county}{County FIPS code}
#'   \item{tract}{Tract FIPS code}
#'   \item{block}{Block FIPS code}
#'   \item{P005003}{White alone population}
#'   \item{P005004}{Black or African American alone population}
#'   \item{P005005}{American Indian and Alaska Native alone population}
#'   \item{P005006}{Asian alone population}
#'   \item{P005007}{Native Hawaiian and Other Pacific Islander alone population}
#'   \item{P005008}{Some other race alone population}
#'   \item{P005009}{Two or more races population}
#'   \item{P005010}{Hispanic or Latino population}
#' }
#'
#' @source Census Bureau via the WRU package.
"rockland_census"
