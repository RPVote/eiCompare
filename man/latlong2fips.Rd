\name{latlong2fips}
\alias{latlong2fips}

\title{
  Latitude-Longitude Coordinates to FIPS Geocode
}
\description{
  Converts latitude/longitude coordinates to 15-digit FIPS code. Communicates with FCC API.
}
\usage{
  latlong2fips(latitude, longitude, number)
}

\arguments{
  \item{latitude}{Numeric. Latitude coordinate.}
  \item{longitude}{Numeric. Longitude coordinate.}
  \item{number}{Numeric. Usually part of a loop index counter}
}

\value{
  Character string 15-digit FIPS code corresponding to Lat/Long entry
}
\references{
  https://geo.fcc.gov/api/census/block/
}
\author{
  Loren Collingwood <loren.collingwood@ucr.edu>
}

\examples{
  
  # EXAMPLE: NOT RUN #
  #census_block <- list()
  #num_catch <- rep(NA, nrow(nom_geo))

  #for (i in 1:nrow(nom_geo)) {
  
  #  census_block[[i]] <- latlong2fips(nom_geo$lat[i], nom_geo$lon[i], i)
  #}
  
  # Row Bind the list into a data.frame object #
  #fips_df <- rbindlist(census_block)
    
}
