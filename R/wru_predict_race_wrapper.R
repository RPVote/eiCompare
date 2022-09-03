#' Prepares a voter file for the WRU predict_race function, and then predicts
#' race.
#'
#' This function assumes that the Census data is provided to the function. It
#' does not provide the capability of downloading the Census data, since this
#' is a time intensive process.
#'
#' @param voter_file The voter file, containing columns with a surname and
#'  potentially geographic information.
#' @param census_data A data frame containing Census data corresponding to the
#'  geographic information for units in the voter file.
#' @param voter_id A string denoting the column containing voter ID. Default is
#'  NULL, if there is no voter ID in the file. In this case, a voter ID will be
#'  assigned.
#' @param surname A string denoting the column containing the surname.
#' @param state A string denoting the column containing the state FIPS code.
#' @param county A string denoting the column containing the county FIPS code.
#' @param tract A string denoting the column containing the tract FIPS code.
#' @param block A string denoting the column containing the block FIPS code.
#' @param census_geo The census level at which to apply BISG. Passed to WRU.
#' @param use_surname Whether to use the surname in calculating race
#'  probabilities. Passed to WRU.
#' @param surname_only Whether to only use the surname in calculating race
#'  probabilities. Passed to WRU.
#' @param surname_year Which Census year to use for surname matching. Passed to
#'  WRU.
#' @param use_age Whether to use the age in the BISG calculation. Passed to WRU.
#' @param use_sex Whether to use the sex in the BISG calculation. Passed to WRU.
#' @param return_surname_flag If TRUE, returns a flag indicating whether the
#'  surnames matched.
#' @param return_geocode_flag If TRUE, returns a flag indicating whether the
#'  first level of geocode matched.
#' @param verbose A flag indicating whether to print out status messages.
#' @return The voter file component extracted from the provided data frame, with
#' additional surname/geocode flags, as well as a data frame race prediction.
#'
#' @references Imai and Khanna (2016) "Improving Ecological Inference by
#' Predicting Individual Ethnicity from Voter Registration Records"
#'
#' @export wru_predict_race_wrapper
#' @import wru
#' @importFrom dplyr relocate
#' @importFrom utils getFromNamespace
wru_predict_race_wrapper <- function(voter_file,
                                     census_data,
                                     voter_id = NULL,
                                     surname = "last_name",
                                     state = NULL,
                                     county = NULL,
                                     tract = NULL,
                                     block = NULL,
                                     census_geo = NULL,
                                     use_surname = TRUE,
                                     surname_only = FALSE,
                                     surname_year = 2010,
                                     use_age = FALSE,
                                     use_sex = FALSE,
                                     return_surname_flag = FALSE,
                                     return_geocode_flag = FALSE,
                                     verbose = FALSE) {
  # Tidy up voter file according to WRU's specifications
  wru_voter_file <- tidy_voter_file_wru(
    voter_file = voter_file,
    voter_id = voter_id,
    surname = surname,
    state = state,
    county = county,
    tract = tract,
    block = block
  )
  
  # Temporary check to force use_sex and use_age into FALSE
  if (use_age) {
    warning("age is currently disabled in wru... forcing use_age to be FALSE")
    use_age <- FALSE
  }
  if (use_sex) {
    warning("sex is currently disabled in wru... forcing use_sex to be FALSE")
    use_sex <- FALSE
  }
  
  # Get merge_surnames function out from wru
  merge_surnames_copy <- utils::getFromNamespace("merge_surnames", "wru")
  
  # If necessary, check which surnames matched
  if (return_surname_flag) {
    if (verbose) {
      message("Matching surnames.")
    }
    merged_surnames <- suppressWarnings(
      merge_surnames_copy(
        voter.file = wru_voter_file,
        surname.year = surname_year,
        clean.surname = TRUE,
        impute.missing = TRUE
      )
    )
    # Get matched surname flag
    voter_file$matched_surname <- !(merged_surnames$surname.match == "")
  }

  # Predict race using BISG via WRU
  if (verbose) {
    message("Performing BISG to obtain race probabilities.")
  }
  invisible(capture.output(
    bisg <- suppressWarnings(
      wru::predict_race(
        voter.file = wru_voter_file,
        census.surname = use_surname,
        surname.only = surname_only,
        surname.year = surname_year,
        census.geo = census_geo,
        census.data = census_data,
        age = use_age,
        sex = use_sex,
      )
    )
  ))
  # Re-order race predictions to match voter file
  bisg <- bisg[match(wru_voter_file$voterid, bisg$voterid), ]
  # Find out which geographic units didn't match
  no_geocode_match <- is.na(bisg$pred.whi)

  # For voters that didn't match geocode, use the next highest level
  if (sum(no_geocode_match) > 1) {
    if (verbose) {
      message(
        "Some voters failed geocode matching. Matching at a higher level."
      )
    }
    no_match_voters <- wru_voter_file[no_geocode_match, ]
    # Re-run BISG using a new geographic unit
    if (census_geo == "block") {
      new_geo <- "tract"
    } else if (census_geo == "tract") {
      new_geo <- "county"
    }
    # Re-run the BISG only on voters that didn't match
    invisible(capture.output(
      bisg_no_match <- suppressWarnings(
        wru::predict_race(
          voter.file = no_match_voters,
          census.surname = use_surname,
          surname.only = surname_only,
          surname.year = surname_year,
          census.geo = new_geo,
          census.data = census_data,
          age = use_age,
          sex = use_sex,
        )
      )
    ))
    # Re-order race predictions to match voter file
    bisg_no_match <- bisg_no_match[match(
      no_match_voters$voterid,
      bisg_no_match$voterid
    ), ]
    # Merge the new probabilities back into the old dataframe
    bisg[no_geocode_match, ] <- bisg_no_match

    matched_geocode <- !no_geocode_match
  } else {
    matched_geocode <- TRUE
  }

  # Store geocode match flag
  if (return_geocode_flag & !surname_only) {
    voter_file$matched_geocode <- matched_geocode
  }

  # Final check if there are no matches
  no_match_final <- is.na(bisg$pred.whi)
  if (sum(no_match_final) > 1) {
    if (verbose) {
      message(paste(
        "Some surnames did not match at higher geographic level.",
        "Using surname only for these cases."
      ))
    }
    # Use probabilities from surnames only for those that don't match
    invisible(capture.output(
      no_match_surnames <- suppressWarnings(
        merge_surnames_copy(
          voter.file = wru_voter_file[no_match_final, ],
          surname.year = surname_year,
          clean.surname = TRUE,
          impute.missing = TRUE
        )
      )
    ))
    # Merge back into BISG estimates
    p_cols <- c("p_whi", "p_bla", "p_his", "p_asi", "p_oth")
    pred_cols <- c("pred.whi", "pred.bla", "pred.his", "pred.asi", "pred.oth")
    bisg[no_match_final, ][, pred_cols] <- no_match_surnames[, p_cols]
  }

  # Finalize voter file
  if (is.null(voter_id)) {
    voter_file$voter_id <- bisg$voterid
    voter_file <- dplyr::relocate(voter_file, voter_id, .before = 1)
  }
  pred_cols <- c("pred.whi", "pred.bla", "pred.his", "pred.asi", "pred.oth")
  voter_file[, pred_cols] <- bisg[, pred_cols]

  if (verbose) {
    message("BISG complete.")
  }
  return(voter_file)
}
