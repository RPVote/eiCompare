#' Function for making basic choropleth maps using the tigris package and ggplot
#'
#' @param voter_file a voter file with latitude, longitude, or latitude/longitude and other geographic data.
#' @param shape_file a shape file based on desired ecologicla unit (i.e. state, county, block, tract)
#' @param crs the Coordinate reference system, default is crs="+proj=latlong +ellps=GRS80 +no_defs"
#' @param title the tile of the map
#'
#' @export shape_file
#' @export shape_list
#'
#' @return Plots of mapped ecological units desired and voter latitude and longitudes
#'
#' @importFrom sf
#' @importFrom ggplot2
#' @importFrom ggmap

map_shape_file <- function(shape_file,
                           crs = "+proj=latlong +ellps=GRS80 +no_defs",
                           title = "Title of the Shapefile") {
  # Map the shapefile only and label ecological unit
  if (!is.null(shape_file)) {
    # Transform shape_file
    shape_file <- st_transform(shape_file, crs = crs)

    suppressMessages(
      sf_points <- st_centroid(shape_file)
    )

    suppressMessages(
      sf_points <- cbind(shape_file, st_coordinates(st_centroid(shape_file$geometry)))
    )

    shape_file <- shape_file %>%
      ggplot() +
      geom_sf() +
      geom_text(
        data = sf_points, aes(x = X, y = Y, label = NAME),
        color = "darkblue", fontface = "bold", check_overlap = FALSE
      ) +
      labs(title) +
      theme_minimal()
  }
  return(shape_file)
}


map_shape_points <- function(voter_file,
                             shape_file,
                             crs = "+proj=longlat +ellps=GRS80",
                             title = "title") {

  # Create lat and lon coordinates from geometry column of the full geometry voter file output
  voter_file_geo_latlon <- tidyr::extract(voter_file, geometry, into = c("lat", "lon"), "\\((.*),(.*)\\)", conv = T)

  # Transform to sf
  voter_file_geo_latlon <- st_as_sf(x = voter_file_geo_latlon, coords = (25:26))


  # Establish Coordinate Reference System (CRS)
  crs <- "+proj=longlat +ellps=GRS80"
  voter_file_geo_latlon <- sf::st_transform(sf::st_set_crs(voter_file_geo_latlon, crs))
  voter_file_geo_latlon <- st_transform(voter_file_geo_latlon, crs = st_crs(shape_file))
  st_crs(voter_file_geo_latlon) <- st_crs(shape_file)

  # Merge files
  voter_file_geo_onlyint <- st_intersection(voter_file_geo_latlon, shape_file)

  # Distinguish between the different counties
  shape_file_polygons <- voter_file_geo_onlyint %>%
    ggplot() +
    geom_sf(aes(fill = factor(COUNTYFP10))) +
    geom_sf(data = shape_file, aes(fill = factor(fips))) +
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
