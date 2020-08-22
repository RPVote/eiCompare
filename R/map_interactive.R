#' This function allows the user to plot an interactive map
#' of the voter longitude and latitude points.
#'
#' @param voter_file a dataframe with a geometry column for
#' latitude and longitudes created after original voter file
#' was processed with a select geocoder.
#' @param voter_id a unique identifier on the voter registration
#' file.
#' @param geometry a column with latitude and longitude points only.
#' For example, the output for latitude and longitude using censusxy
#' is a column called geometry with the value structure, c(latitude,
#' longitude). This is not a column with multipolygon geometries like
#' in a shapefile.
#' @param lat the column (or vector) with latitude points.
#' @param lon the column (or vector) with longitude points.
#' @param first_name the column with first names of voters.
#' @param last_name the column with last names of voters.
#' @param fips_code the column with the fips code for the designated
#' geograhic.
#' unit of interest (i.e. state, county, block, tract).
#' @param latitude the column of the of the voter_file that
#' corresponds to #' latitude coordinates. This is optional
#' and a parameter only used if the dataframe used does not
#' have a concatenated geometry column with a
#' "c(latitude, longitude)" structure as in the output from the
#' geocoder censusxy
#'
#' @importFrom tidyr extract
#' @importFrom leaflet addTiles addMarkers
#' @export map_interactive
#'
#' @author Loren Collingwood <loren.collingwood@@ucr.edu>
#' @author Juandalyn Burke <jcburke@@uw.edu>

map_interactive <- function(voter_file,
                            voter_id = "id",
                            f_name = "firstname",
                            l_name = "lastname",
                            fips_code = "countycode",
                            latitude = "lat",
                            longitude = "lon") {
  if (class(voter_file) == "data.frame" & any(colnames(voter_file) == "geometry")) {
    latlon_df <- tidyr::extract(voter_file,
      geometry,
      into = c("lat", "lon"), "\\((.*),(.*)\\)",
      conv = T
    )
    latitude <- latlon_df$lat
    longitude <- latlon_df$lon
  }
  if (class(voter_file) == "data.frame" & !is.null(latitude) & !is.null(longitude)) {
    latlon_df <- voter_file
  }

  leaflet(data = latlon_df) %>%
    addTiles() %>%
    addMarkers(~ latlon_df[[latitude]], ~ latlon_df[[longitude]],
      popup = paste(
        "Voter ID:", latlon_df[[voter_id]], "<br>",
        "First Name:", latlon_df[[first_name]], "<br>",
        "Last Name:", latlon_df[[last_name]], "<br>",
        "FIPS code:", latlon_df[[fips_code]], "<br>",
        "Latitude:", latlon_df[[latitude]], "<br>",
        "Longitude:", latlon_df[[longitude]], "<br>"
      )
    )
}
