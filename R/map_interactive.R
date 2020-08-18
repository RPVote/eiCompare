#' This script allows the user to plot an interactive map of the voter longitude and latitude points.
#'
#' @param sp_voterfile a spatial dataframe (sp) with geometry column created after original voter file was processed with a select geocoder.
#' @param voter_id a unique identifier on the voter registration file.
#' @param geometry a column with latitude and longitude points.
#' @param lat the column (or vector) with latitude points
#' @param lon the column (or vector) with longitude points
#' @param first_name the column with first names of voters.
#' @param last_name the column with last names of voters.
#' @param fips the column with the fips code for the designated geograhic unit of interest (i.e. state, county, block, tract)
#'
#' @export inter_map a map of voter's latitude and longitude points with voter information pop-ups.
#'
#' @importFrom tidyr
#' @importFrom leaflet

sp_latlon <- tidyr::extract(sp_voterfile, geometry, into = c("lat", "lon"), "\\((.*),(.*)\\)", conv = T)

leaflet(data = sp_latlon) %>%
  addTiles() %>%
  addMarkers(~lon, ~lat,
    popup = paste(
      "Voter ID", voter_id, "<br>",
      "First Name:", first_name, "<br>",
      "Last Name:", last_name, "<br>",
      "FIPS code:", fips_code, "<br>",
      "Latitude:", lat, "<br>",
      "Longitude:", lon, "<br>"
    )
  )
