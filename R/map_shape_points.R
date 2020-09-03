#' Function for making basic choropleth maps of longitude and latitude
#' points using the tigris package and ggplot
#'
#' @param voter_file a voter file with latitude, longitude, or
#' latitude/longitude and other geographic data.
#' @param shape_file a shape file based on desired ecological
#' unit (i.e. state, county, block, tract)
#' @param crs the Coordinate reference system, default is
#' crs="+proj=latlong +ellps=GRS80 +no_defs"
#' @param title the tile of the map
#'
#' @export map_shape_file
#'
#' @return Plots of mapped ecological units desired and voter 
#' latitude and longitudes
#'
#' @import ggplot2
#' @importFrom sf st_transform st_centroid st_coordinates st_intersection 
#' st_crs st_as_sf
#' @importFrom tidyr extract
#'
#' @author Loren Collingwood <loren.collingwood@@ucr.edu>
#' @author Juandalyn Burke <jcburke@@uw.edu>

map_shape_points <- function(voter_file,
         shape_file,
         crs = "+proj=longlat +ellps=GRS80",
         title = "title") {
    
    # Convert Shape file to sf (usually from sp)
    shape_file <- sf::st_as_sf (shape_file)
    shape_file <- sf::st_transform(shape_file, crs = crs)
    
    # Create lat and lon coordinates from geometry column of the
    # full geometry voter file output
    voter_file_geo_latlon <- tidyr::extract(
        voter_file,
        "geometry",
        into = c("lat", "lon"),
        "\\((.*),(.*)\\)",
        conv = TRUE
    )
    
    # Transform to sf
    voter_file_geo_latlon <- sf::st_as_sf(
        x = voter_file_geo_latlon,
        coords = c("lat", "lon")
    )
    
    # Establish Coordinate Reference System (CRS)
    voter_file_geo_latlon <- sf::st_transform(
        sf::st_set_crs(voter_file_geo_latlon, crs)
    )
    
    voter_file_geo_latlon <- sf::st_transform(
        voter_file_geo_latlon,
        crs = sf::st_crs(shape_file)
    )
    sf::st_crs(voter_file_geo_latlon) <- sf::st_crs(shape_file)
    
    # Merge files
    
    suppressMessages(
    suppressWarnings(
        voter_file_geo_onlyint <- sf::st_intersection(
            voter_file_geo_latlon,
            shape_file
        )
    )
    )
    # Distinguish between the different counties
    shape_file_polygons <- voter_file_geo_onlyint %>%
        ggplot() +
        geom_sf(aes_string(fill = factor("COUNTYFP10"))) +
        geom_sf(data = shape_file, aes_string(fill = factor("fips"))) +
        labs(fill = "COUNTYFP10") +
        theme_minimal()
    
    # Map the geometry points over the shapefile
    shape_file_points <- ggplot() +
        geom_sf(data = shape_file, fill = "gray36") +
        geom_sf(
            data = voter_file_geo_onlyint,
            col = "orange",
            size = 1.5
        ) +
        scale_fill_grey(start = 0.3, end = 0.3) +
        labs(title = title)
    
    shape_list <- list(shape_file_polygons, shape_file_points)
    
    return(shape_list)
}