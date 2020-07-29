#' De-duplicates a voter file.
#'
#' Currently, this function removes all but the latest entries in a voter file
#' according to voter ID. This assumes the voter file is sorted by voter ID
#' chronologically.
#'
#' This function can be updated with more functionality to handle edge cases.
#'
#' @param voter_file The voter file, as a data frame or tibble.
#' @param voter_id The column denoting the voter ID.
#' @return The voter file with duplicates removed.
#'
#' @export dedupe_voter_file
dedupe_voter_file <- function(voter_file, voter_id = "voter_id") {
  # obtain the earliest duplicates in the voter file
  duplicates <- duplicated(voter_file[, voter_id], fromLast = TRUE)
  return(voter_file[!duplicates, ])
}


#' Merges a voter file to a shape file.
#'
#' This is achieved by determining the units (e.g., Census block, district,
#' etc.) for which each voter's address lies within.
#'
#' This function assumes that the sf package was used to read in the shape
#' files.
#'
#' @param voter_file A dataframe denoting the voter file. If it is not a
#'  geometry dataframe, it will be converted to one.
#' @param shape_file The shapefile for the region, as an sf object.
#' @param crs The PROJ4 string for the coordinate reference system.
#' @param coords The columns, as a list, that refer to the longitude and
#'  latitude.
#' @param voter_id The column for the Voter ID.
#' @return The voter file with unit information attached.
#'
#' @export merge_voter_file_to_shape
#' @importFrom sf st_as_sf st_join st_set_crs st_transform
merge_voter_file_to_shape <- function(voter_file,
                                      shape_file,
                                      crs = "+proj=longlat +ellps=GRS80",
                                      coords = c("lon", "lat"),
                                      voter_id = "voter_id") {
  # apply transform to the voter file longitude and latitude
  voter_file <- sf::st_as_sf(voter_file, coords = coords)
  voter_file <- sf::st_transform(sf::st_set_crs(voter_file, crs))

  # apply transform to the shapefile
  shape_file <- sf::st_transform(shape_file, crs = crs)

  # find the intersection between the voter file addresses
  voter_file_w_shape <- suppressMessages(sf::st_join(
    x = voter_file,
    y = shape_file
  ))
  # remove duplicates
  voter_file_w_shape <- dedupe_voter_file(voter_file_w_shape,
    voter_id = voter_id
  )
  return(voter_file_w_shape)
}
