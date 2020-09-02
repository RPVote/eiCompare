map_interactive <-  function(voter_file,
         
         voter_id = "registration_number",
         f_name = "first_name",
         l_name = "last_name",
         fips_code = "county_code",
         latitude = "lat",
         longitude = "lon") {
    
    if (class(voter_file) == "data.frame" & any(colnames(voter_file) == "geometry")) {
        voter_file <- tidyr::extract(voter_file,
                                     .data$geometry,
                                     into = c("lat", "lon"), "\\((.*),(.*)\\)",
                                     conv = T
        )
        voter_file$latitude <- voter_file$lat
        voter_file$longitude <- voter_file$lon
    }
    
    
    map_inter <- leaflet(data = voter_file) %>%
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