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


#' Tidies a voter file for WRU.
#'
#' Checks if columns exist in the original voter file and renames them so that
#' WRU can process the new voter file. Only extract the information needed,
#' tossing the remaining columns.
#'
#' @param voter_file The voter file, as a data frame or tibble.
#' @param voter_id A string denoting the column containing voter ID. Default is
#'  NULL, when the voter file does not have an ID or registration number.
#' @param surname A string denoting the column containing the surname.
#' @param state A string denoting the column containing the state FIPS code.
#' @param county A string denoting the column containing the county FIPS code.
#' @param tract A string denoting the column containing the tract FIPS code.
#' @param block A string denoting the column containing the block FIPS code.
#' @return A new voter file that can be read in by WRU functions.
#'
#' @export tidy_voter_file_wru
tidy_voter_file_wru <- function(voter_file,
                                voter_id = NULL,
                                surname = NULL,
                                state = NULL,
                                county = NULL,
                                tract = NULL,
                                block = NULL) {
  # Create voter file, making new voter IDs if necessary
  if (is.null(voter_id)) {
    voter_id <- seq_len(nrow(voter_file))
  } else {
    voter_id <- voter_file[, voter_id]
  }
  # Create new voter file according to WRU's specifications. State is handled
  # here because we assume that only one state is queried.
  new_voter_file <- data.frame(voterid = voter_id, state = state)

  # Add surnames to new voter file
  if (!is.null(surname)) {
    new_voter_file[["surname"]] <- tolower(voter_file[[surname]])
  }

  # Add geographic units
  if (any(names(voter_file) == county)) {
    new_voter_file[["county"]] <- as.character(voter_file[[county]])
  }
  if (any(names(voter_file) == tract)) {
    new_voter_file[["tract"]] <- as.character(voter_file[[tract]])
  }
  if (any(names(voter_file) == block)) {
    new_voter_file[["block"]] <- as.character(voter_file[[block]])
  }
  return(new_voter_file)
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
#' @param crs The PROJ4 string or int for the coordinate reference system.
#' @param coords The columns, as a list, that refer to the longitude and
#'  latitude.
#' @param voter_id The column for the Voter ID.
#' @return The voter file with unit information attached.
#'
#' @export merge_voter_file_to_shape
#' @importFrom sf st_as_sf st_join st_set_crs st_transform
merge_voter_file_to_shape <- function(voter_file,
                                      shape_file,
                                      crs = NULL,
                                      coords = c("lon", "lat"),
                                      voter_id = "voter_id") {
  # Apply transform to the voter file longitude and latitude
  voter_file <- sf::st_as_sf(voter_file, coords = coords)

  # If no provided CRS, use the shape file's CRS
  if (is.null(crs)) {
    if (is.na(sf::st_crs(shape_file))) {
      # Shape file has no CRS
      crs <- 4326
      shape_file <- sf::st_set_crs(shape_file, crs)
    } else {
      crs <- sf::st_crs(shape_file)
    }
    voter_file <- sf::st_transform(sf::st_set_crs(voter_file, crs))
  } else {
    # Use the provided CRS for both voter and shape files
    crs <- sf::st_crs(crs)
    voter_file <- sf::st_transform(sf::st_set_crs(voter_file, crs))
    shape_file <- sf::st_transform(shape_file, crs = crs)
  }

  # Find the intersection between the voter file addresses
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
