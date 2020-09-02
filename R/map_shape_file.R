map_shape_file <-  function(shape_file,
                             crs = "+proj=latlong +ellps=GRS80 +no_defs",
                             title = "Title of the Shapefile") {
    
    # Map the shapefile only and label ecological unit
    if (!is.null(shape_file)) {
        
        # Transform shape_file
        shape_file <- sf::st_as_sf (shape_file)
        shape_file <- sf::st_transform(shape_file, crs = crs)
        
        suppressWarnings(
            sf_points <- sf::st_centroid(shape_file)
        )
        
        suppressWarnings(
            sf_points <- cbind(
                shape_file,
                sf::st_coordinates(sf::st_centroid(shape_file$geometry))
            )
        )
        
        shape_file <- shape_file %>%
            ggplot() +
            geom_sf() +
            geom_text(
                data = sf_points, aes_string(x = "X", y = "Y", label = "NAME"),
                color = "darkblue", fontface = "bold", check_overlap = FALSE
            ) +
            labs(title) +
            theme_minimal()
    }
    return(shape_file)
}