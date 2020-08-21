#' This script allows the user to plot an interactive map of the voter
#' longitude and latitude points.
#'
#' @param voter_file an dataframe with a geometry column for latitude and
#' longitudes created after original voter file was processed with a select
#' geocoder.
#' @param voter_id a unique identifier on the voter registration file.
#' @param geometry a column with latitude and longitude points only.
#' This is not a column with multipolygon geometries like in a shapefile.
#' @param lat the column (or vector) with latitude points
#' @param lon the column (or vector) with longitude points
#' @param first_name the column with first names of voters.
#' @param last_name the column with last names of voters.
#' @param fips the column with the fips code for the designated geograhic
#' unit of interest (i.e. state, county, block, tract)
#'
#' @export map_interactive
#' @import tidyr
#' @import leaflet
#'
#' @author Loren Collingwood <loren.collingwood@@ucr.edu>
#' @author Juandalyn Burke <jcburke@@uw.edu>

map_interactive <- function(voter_file,
                            voter_id = "id",
                            f_name = "firstname",
                            l_name = "lastname",
                            fips_code = "countycode") {
  if (class(voter_file) == "data.frame" & any(colnames == "geometry")) {
    latlon_df <- tidyr::extract(voter_file,
      geometry,
      into = c("lat", "lon"), "\\((.*),(.*)\\)",
      conv = T
    )

    leaflet(data = latlon_df) %>%
      addTiles() %>%
      addMarkers(~lat, ~lon,
        popup = paste(
          "Voter ID:", latlon_df[[voter_id]], "<br>",
          "First Name:", latlon_df[[first_name]], "<br>",
          "Last Name:", latlon_df[[last_name]], "<br>",
          "FIPS code:", latlon_df[[fips_code]], "<br>",
          "Latitude:", latlon_df[[lat]], "<br>",
          "Longitude:", latlon_df[[lon]], "<br>"
        )
      )
  }
}
