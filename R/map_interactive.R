#' This function allows the user to plot an interactive map
#' of the voter longitude and latitude points.
#'
#' @param voter_file a dataframe with a geometry column for
#' latitude and longitudes created after original voter file
#' was processed with a select geocoder.
#' @param voter_id a unique identifier on the voter registration
#' file.
#' @param f_name the column with first names of voters.
#' @param l_name the column with last names of voters.
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
#' @param longitude the column of the of the voter_file that
#' corresponds to #' longitude coordinates. This is optional
#' and a parameter only used if the dataframe used does not
#' have a concatenated geometry column with a
#' "c(latitude, longitude)" structure as in the output from the
#' geocoder censusxy
#' 
#' @return A leaflet html widget object
#' @importFrom tidyr extract
#' @importFrom rlang .data
#' @importFrom leaflet addTiles addMarkers
#' @export map_interactive
#'
#' @author Loren Collingwood <loren.collingwood@@ucr.edu>
#' @author Juandalyn Burke <jcburke@@uw.edu>

map_interactive <-  function(voter_file,
         
         voter_id = "registration_number",
         f_name = "first_name",
         l_name = "last_name",
         fips_code = "county_code",
         latitude = "lat",
         longitude = "lon") {
    
    if (is.data.frame(voter_file) & any(colnames(voter_file) == "geometry")) {
        voter_file <- tidyr::extract(voter_file,
                                     .data$geometry,
                                     into = c("lat", "lon"), "\\((.*),(.*)\\)",
                                     conv = T
        )
        voter_file$latitude <- voter_file$lat
        voter_file$longitude <- voter_file$lon
    }
    
    
    map_inter <- leaflet::leaflet(data = voter_file) %>%
        addTiles() %>%
        addMarkers(~lat, ~lon,
                   popup = paste(
                       "Voter ID:", voter_file[[voter_id]], "<br>",
                       "First Name:", voter_file[[f_name]], "<br>",
                       "Last Name:", voter_file[[l_name]], "<br>",
                       "FIPS code:", voter_file[[fips_code]], "<br>",
                       "Latitude:", voter_file[[latitude]], "<br>",
                       "Longitude:", voter_file[[longitude]], "<br>"
                   )
        )
    
    return(map_inter)
}