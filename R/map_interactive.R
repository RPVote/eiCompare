#' This script allows the user to plot an interactive map of the voter
#' longitude and latitude points.
#'
#' @param voterfile an dataframe with a geometry column for latitude and
#' longitudes created after original voter file was processed with a select
#' geocoder.
#' @param voter_id a unique identifier on the voter registration file.
#' @param geometry a column with latitude and longitude points.
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

map_interactive <- function(voterfile,
                            voter_id = "id",
                            f_name = "firstname",
                            l_name = "lastname",
                            fips_code = "countycode") {
  if (class(voterfile) == "data.frame") {
    latlon_df <- tidyr::extract(voterfile,
      geometry,
      into = c("lat", "lon"), "\\((.*),(.*)\\)",
      conv = T
    )

    leaflet(data = sp_latlon) %>%
      addTiles() %>%
      addMarkers(~lat, ~lon,
        popup = paste(
          "Voter ID:", registration_number, "<br>",
          "First Name:", first_name, "<br>",
          "Last Name:", last_name, "<br>",
          "FIPS code:", fips_code, "<br>",
          "Latitude:", lat, "<br>",
          "Longitude:", lon, "<br>"
        )
      )
  }
}
