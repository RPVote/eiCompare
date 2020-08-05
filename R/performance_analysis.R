#' Performs a performance analysis using a voter file, census shape, and
#' district shape.
#'
#' @param voter_file A dataframe containing the voter file.
#' @param census_data A dataframe containing the Census tracts or blocks in the
#' region for the voter file.
#' @param census_shape The shapefiles for the Census blocks or tracts for which
#' the voter file will be geocoded against.
#' @param district_shape The shapefiles for the new districts or precincts to
#' consider.
#' @param state The state in which the functionality analysis is performed, as
#'   a two character string.
#' @param voter_id A string denoting the column name for the voter ID.
#' @param surname A string denoting the column name for the surname.
#' @param district A string denoting the column name for the district.
#' @param census_state_col The column in the Census data that indicates state.
#' @param census_county_col The column in the Census data that indicates county.
#' @param census_tract_col The column in the Census data that indicates tract.
#' @param census_block_col The column in the Census data that indicates block.
#' @param census_fips_col The column in the Census data for the FIPS code.
#' @param crs A string denoting the PROJ4 string for projecting maps.
#' @param coords The columns for the coordinates.
#' @param census_geo The geographic level at which to perform BISG.
#' @param use_surname Whether to use the surname in calculating race
#'   probabilities. Passed to WRU.
#' @param surname_only Whether to only use the surname in calculating race
#'   probabilities. Passed to WRU.
#' @param surname_year Which Census year to use for surname matching. Passed to
#'   WRU.
#' @param use_age Whether to use the age in the BISG calculation. Passed to WRU.
#' @param use_sex Whether to use the sex in the BISG calculation. Passed to WRU.
#' @param normalize If TRUE, normalizes the district percentages.
#' @param verbose If TRUE, will output diagnostic strings.
#' @return The processed voter file and a summary of district turnout across
#'   racial groups.
#'
#' @export performance_analysis
#' @importFrom dplyr filter group_by_at inner_join rename select summarise
#' @importFrom tidyselect all_of
performance_analysis <- function(voter_file,
                                 district_shape,
                                 census_data,
                                 census_shape,
                                 join_census_shape = TRUE,
                                 join_census_race = TRUE,
                                 join_district_shape = TRUE,
                                 state = NULL,
                                 voter_id = "voter_id",
                                 surname = "last_name",
                                 district = "district",
                                 census_state_col = "STATEFP10",
                                 census_county_col = "COUNTYFP10",
                                 census_tract_col = "TRACTCE10",
                                 census_block_col = "BLOCKCE10",
                                 census_fips_col = "GEOID10",
                                 crs = "+proj=longlat +ellps=GRS80",
                                 coords = c("lon", "lat"),
                                 census_geo = "block",
                                 use_surname = TRUE,
                                 surname_only = FALSE,
                                 surname_year = 2010,
                                 use_age = FALSE,
                                 use_sex = FALSE,
                                 normalize = TRUE,
                                 verbose = FALSE) {
  if (verbose) {
    n_voters <- nrow(voter_file)
    message(paste(
      "Voter file has", n_voters, "rows.\nBeginning",
      "functionality analysis..."
    ))
  }

  # De-duplicate voter file
  voter_file <- dedupe_voter_file(
    voter_file = voter_file,
    voter_id = voter_id
  )
  if (verbose) {
    n_voters_new <- nrow(voter_file)
    message(paste(
      "De-duplicating removed", n_voters - n_voters_new, "voters.",
      "Voter file now has", n_voters_new, "rows.\nMerging",
      "voter file with census shape files."
    ))
    n_voters <- n_voters_new
  }

  # Merge the voter file to census shape file, then pick out the right columns
  if (join_census_shape) {
    voter_file_w_census <- merge_voter_file_to_shape(
      voter_file = voter_file,
      shape_file = census_shape,
      crs = crs,
      coords = coords,
      voter_id = voter_id
    )
    # Filter out voters that didn't match on the block
    voter_file_w_census <- dplyr::filter(
      voter_file_w_census,
      !is.na(.data[[census_block_col]])
    )

    if (verbose) {
      n_voters_new <- nrow(voter_file_w_census)
      message(paste(
        "Matching by Census block removed", n_voters - n_voters_new,
        "voters. Voter file now has", n_voters_new,
        "rows.\nMerging voter file with district shape files."
      ))
      n_voters <- n_voters_new
    }
  } else {
    # If we don't need to merge to Census shape file, then rename variable
    if (verbose) {
      message(paste("Voter file already matched to Census shapefile."))
    }
    voter_file_w_census <- voter_file
  }

  if (join_district_shape) {
    # Merge the voter file with the district shape file
    voter_file_w_district <- merge_voter_file_to_shape(
      voter_file = voter_file_w_census,
      shape_file = district_shape,
      crs = crs,
      coords = coords,
      voter_id = voter_id
    )
    # Filter out voters that didn't match on a district
    voter_file_w_district <- dplyr::filter(
      voter_file_w_district,
      !is.na(.data[[district]])
    )
    if (verbose) {
      n_voters_new <- nrow(voter_file_w_district)
      message(paste(
        "Matching by district removed", n_voters - n_voters_new,
        "voters. Voter file now has", n_voters_new,
        "rows.\nApplying BISG."
      ))
      n_voters <- n_voters_new
    }
  } else {
    if (verbose) {
      # If we don't need to merge to district shape file, then rename variable
      message(paste("Voter file already matched to district shape."))
      voter_file_w_district <- voter_file
    }
  }

  # Select the final set of columns needed for BISG
  voter_file_final <- voter_file_w_district %>%
    dplyr::rename(
      c(
        "st" = tidyselect::all_of(census_state_col),
        "county" = tidyselect::all_of(census_county_col),
        "tract" = tidyselect::all_of(census_tract_col),
        "block" = tidyselect::all_of(census_block_col),
        "fips" = tidyselect::all_of(census_fips_col)
      )
    ) %>%
    dplyr::select(
      tidyselect::all_of(c(
        voter_id,
        surname,
        district,
        "st",
        "county",
        "tract",
        "block"
      ))
    )

  # Apply BISG to the voter file to get race predictions
  bisg <- wru_predict_race_wrapper(
    voter_file = as.data.frame(voter_file_final),
    census_data = census_data,
    voter_id = voter_id,
    surname = surname,
    state = state,
    county = "county",
    tract = "tract",
    block = "block",
    census_geo = census_geo,
    use_surname = use_surname,
    surname_only = surname_only,
    surname_year = surname_year,
    use_age = use_age,
    use_sex = use_sex,
    return_surname_flag = TRUE,
    return_geocode_flag = TRUE,
    verbose = TRUE
  )
  # Add matching flags to voter file
  voter_file_final$matched_surname <- bisg$voter_file$matched_surname
  voter_file_final$matched_geocode <- bisg$voter_file$matched_geocode
  if (verbose) {
    n_surname_match <- sum(bisg$voter_file$matched_surname)
    n_geocode_match <- sum(bisg$voter_file$matched_geocode)
    message(paste0(
      paste("BISG didn't match", n_voters - n_surname_match, "surnames.\n"),
      paste("BISG didn't match", n_voters - n_geocode_match, "geocodes.")
    ))
  }

  # Merge race information back into voter file
  voter_file_final_w_race <- dplyr::inner_join(
    x = as.data.frame(voter_file_final),
    y = bisg$bisg[, c(
      "voterid",
      "pred.whi",
      "pred.bla",
      "pred.his",
      "pred.asi",
      "pred.oth"
    )],
    by = setNames("voterid", voter_id)
  )

  # Aggregate percentages across districts
  results <- voter_file_final_w_race %>%
    dplyr::group_by_at(district) %>%
    dplyr::summarise(
      "white" = mean(pred.whi),
      "black" = mean(pred.bla),
      "hispanic" = mean(pred.his),
      "asian" = mean(pred.asi),
      "other" = mean(pred.oth),
      .groups = "drop"
    )

  # If necessary, normalize counts
  if (normalize) {
    races <- c("white", "black", "hispanic", "asian", "other")
    sums <- rowSums(results[, races])
    results[, races] <- results[, races] / sums
  }

  if (verbose) {
    message("Performance analysis complete.")
  }
  return(list(
    voter_file = voter_file_final_w_race,
    results = results
  ))
}
