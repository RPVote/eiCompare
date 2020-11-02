#' Gets identifier for geographic unit at the level above the provided unit.
#'
#' This function accept a Census geographic identifier and returns the
#' identifier one level above the provided one, in terms of spatial coverage.
#'
#' @param census_geo A string indicating the geographic level.
#' @return A string indicating the geographic unit one level above the provided
#'  unit.
#'
#' @export swap_census_geography
swap_census_geography <- function(census_geo) {
  if (census_geo == "block") {
    new_geo <- "block group"
  } else if (census_geo == "block group") {
    new_geo <- "tract"
  } else if (census_geo == "tract") {
    new_geo <- "county"
  } else if (census_geo == "county") {
    new_geo <- "state"
  } else {
    stop("Incorrect Census geographic identifier.")
  }
  return(new_geo)
}


#' Gets Census race counts necessary for BISG.
#'
#' This function gets the columns necessary to run BISG in compliance with
#' the Census' Bureau's surname lists. It can extract columns for either the
#' decennial Census or the ACS.
#'
#' @param geography The geographic level at which to obtain Census data. If
#'  obtaining data from the decennial Census, can be up to "block". If
#'  obtaining data from the ACS, can only be up to "block group".
#' @param state The state from which to obtain Census data, as a string.
#' @param county The county(ies) from which to obtain Census data. If NULL,
#'  data is obtained from all counties in the state.
#' @param year The year to obtain Census data from. If 2010, uses decennial
#'  data. Otherwise, uses the 5-year ACS summary data.
#' @param cache A bool denoting whether the Census data should be cached.
#' @return A tibble containing the race counts per geography unit for white,
#'  black, Hispanic/Latino, Asian, and other voters.
#'
#' @import dplyr
#' @importFrom tidycensus get_decennial get_acs
#' @importFrom tigris list_counties
#' @export get_census_race_counts
get_census_race_counts <- function(
  geography, state, county = NULL, year = 2010, cache = FALSE
) {
  # Get FIPS county codes if they're not provided
  if (is.null(county)) {
    # Filter county codes from tidycensus by state
    county <- tigris::list_counties(state)[["county_code"]]
  }
  # Decennial data
  if (year == 2010) {
    # Variable names for SF1 dataset
    variables <- c("P005003",
                   "P005004",
                   "P005005",
                   "P005006",
                   "P005007",
                   "P005008",
                   "P005009",
                   "P005010")
    # Use tidycensus to extract Census counts
    counts <- suppressMessages(tidycensus::get_decennial(
      geography = geography,
      variables = variables,
      year = year,
      state = state,
      county = county,
      output = "wide",
      cache_table = cache)) %>%
      dplyr::mutate(
        fips = GEOID,
        whi = P005003,
        bla = P005004,
        his = P005010,
        asi = P005006 + P005007,
        oth = P005005 + P005008 + P005009) %>%
      dplyr::select(fips, whi, bla, his, asi, oth)
  } else {
    # Variable names for ACS
    variables <- c("B03002_003",
                   "B03002_004",
                   "B03002_005",
                   "B03002_006",
                   "B03002_007",
                   "B03002_008",
                   "B03002_009",
                   "B03002_010",
                   "B03002_011",
                   "B03002_012")
    # Use tidycensus to extract counts for the ACS
    counts <- suppressMessages(tidycensus::get_acs(
      geography = geography,
      variables = variables,
      year = year,
      output = "wide",
      state = state,
      county = county,
      survey = "acs5",
      cache_table = cache)) %>%
      dplyr::mutate(
        fips = GEOID,
        whi = B03002_003E,
        bla = B03002_004E,
        his = B03002_012E,
        asi = B03002_006E + B03002_007E,
        oth = B03002_008E + B03002_009E + B03002_010E + B03002_011E
      ) %>%
      dplyr::select(
        fips, whi, bla, his, asi, oth
      )
  }
  return(counts)
}


#' Computes the probability a person is located in a specific geographic unit.
#'
#' This is a utility function for performing BISG. It operates on a dataframe
#' obtained from the Census Bureau via the provided eiCompare helper function.
#'
#' @param counts A tibble containing counts (divided amongst constituent groups)
#'  per geographic units (rows).
#' @param cols The columns denoting the constituent groups within each
#'  geographic units.
#' @return A vector containing the probability of selecting a person from each
#'  geographic unit.
#'
#' @export compute_p_g
compute_p_g <- function(counts, cols = c("whi", "bla", "his", "asi", "oth")) {
  total_by_geo <- rowSums(counts[, cols])
  p_g <- total_by_geo / sum(total_by_geo)
  return(p_g)
}


#' Computes the probability a person is of a specific racial group, conditioned
#' on geographic unit.
#'
#' This is a utility function for performing BISG. It operates on a dataframe
#' obtained from the Census Bureau via the provided eiCompare helper function.
#'
#' @param counts A tibble containing counts (divided amongst constituent groups)
#'  per geographic units (rows).
#' @param cols The columns denoting the constituent groups within each
#'  geographic units.
#' @return A tibble with dimensions equal to counts, with each entry denoting
#'  the probability of selecting a person per racial group (columns) conditioned
#'  on racial group (rows).
#'
#' @importFrom dplyr across mutate
#' @export compute_p_r_cond_g
compute_p_r_cond_g <- function(
  counts, cols = c("whi", "bla", "his", "asi", "oth")
) {
  p_r_g <- counts %>%
    dplyr::mutate(
      dplyr::across(.cols = cols, .fns = ~ . / rowSums(counts[, cols]))
    ) %>%
    dplyr::mutate(dplyr::across(.cols = cols, .fns = ~ ifelse(is.na(.), 0, .)))
  return(p_r_g)
}


#' Computes the probability a person is in a specific geographic unit,
#' conditioned on a racial group.
#'
#' This is a utility function for performing BISG. It operates on a dataframe
#' obtained from the Census Bureau via the provided eiCompare helper function.
#'
#' @param counts A tibble containing counts (divided amongst constituent groups)
#'  per geographic units (rows).
#' @param cols The columns denoting the constituent groups within each
#'  geographic units.
#' @return A tibble with dimensions equal to counts, with each entry denoting
#'  the probability of selecting a person per geographic unit (rows)
#'  conditioned on racial group (columns).
#'
#' @importFrom dplyr across all_of mutate
#' @export compute_p_r_cond_g
compute_p_g_cond_r <- function(
  counts, cols = c("whi", "bla", "his", "asi", "oth")
) {
  # Need P(G) and P(R|G) to invert for P(G|R) according to Bayes' Theorem
  p_g <- compute_p_g(counts, cols)
  p_r_g <- compute_p_r_cond_g(counts, cols)
  # Apply Bayes' Theorem
  p_g_r <- p_r_g %>%
    dplyr::mutate(
      dplyr::across(.cols = cols, .fns = ~ . * dplyr::all_of(p_g))
    ) %>%
    dplyr::mutate(dplyr::across(.cols = cols, .fns = ~ . / sum(.)))
  return(p_g_r)
}


#' Computes the probability a person is of a specific racial group, conditioned
#' on surname.
#'
#' This is a utility function for performing BISG. It operates on a voter file,
#' and utilizes WRU's function for calculating probability of race by surname.
#'
#' @param voter_file A tibble containing a list of voters (by row), and a
#'  column that denotes their surname.
#' @param surname_col A string denoting which column contains the voter surname.
#' @return A tibble with rows denoting voters and columns denoting the
#'  probability that each voter is of a particular racial group.
#'
#' @import dplyr
#' @importFrom wru merge_surnames
#' @export compute_p_r_cond_s
compute_p_r_cond_s <- function(voter_file, surname_col) {
  p_r_s <- suppressWarnings(suppressMessages(wru::merge_surnames(
    voter.file = dplyr::rename(voter_file,
                               surname = dplyr::all_of(surname_col)),
    surname.year = 2010,
    clean.surname = TRUE,
    impute.missing = TRUE
  ))) %>%
    dplyr::rename(
      !!surname_col := surname,
      whi = p_whi,
      bla = p_bla,
      his = p_his,
      asi = p_asi,
      oth = p_oth
    ) %>%
    dplyr::select(
      all_of(surname_col), whi, bla, his, asi, oth
    )
  return(p_r_s)
}


#' Computes the probability a person is of a specific racial group, conditioned
#' on surname and geolocation.
#'
#' This is a utility function for performing BISG. It operates on a voter file,
#' and counts obtained from the Census Bureau via eiCompare's helper function.
#'
#' @param voter_file A tibble containing a list of voters (by row), and a
#'  column that denotes their surname.
#' @param counts A tibble containing counts (divided amongst constituent groups)
#'  per geographic units (rows).
#' @param surname_col A string denoting which column contains the voter surname.
#' @param geo_col A string denoting which column contains the geographic unit
#'  ID.
#' @param race_cols A list of strings denoting the columns containing racial
#'  groups.
#' @param geo_col_counts A string denoting the column in the counts tibble that
#'  refers to the geographic unit.
#' @param p_r_s A dataframe denoting the probability of race conditioned on
#'  surname that matches with the provided voter file. Can accelerate
#'  computation if provided up front.
#'
#' @return A tibble with rows denoting voters and columns denoting the
#'  probability that each voter is of a particular racial group.
#'
#' @import dplyr
#' @importFrom stats setNames
#' @importFrom stringr str_length str_sub
#' @export compute_p_r_cond_s_g
compute_p_r_cond_s_g <- function(
  voter_file, counts, surname_col, geo_col,
  race_cols = c("whi", "bla", "his", "asi", "oth"), geo_col_counts = "fips",
  p_r_s = NULL
) {
  if (is.null(p_r_s)) {
    # Compute probability of race conditioned on surname
    p_r_s <- compute_p_r_cond_s(voter_file = voter_file,
                                surname_col = surname_col)
  }
  # Compute probability of geography conditioned on race, for all geographies
  p_g_r_all <- compute_p_g_cond_r(counts = counts, cols = race_cols)
  # Check if the IDs for voter file and the counts have the same length
  fips_length <- unique(stringr::str_length(counts[[geo_col_counts]]))
  voter_file[[geo_col]] <- stringr::str_sub(
    voter_file[[geo_col]],
    end = fips_length)
  # Merge p(G|R) into the voter file by FIPS code
  p_g_r <- dplyr::left_join(
    x = voter_file,
    y = p_g_r_all,
    by = stats::setNames(geo_col_counts, geo_col)
  )
  # Compute the final probability of race
  p_r_s_g <- (p_r_s[, race_cols] * p_g_r[, race_cols]) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(total = sum(dplyr::across(.cols = race_cols))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(dplyr::across(.cols = race_cols, .fns = ~ . / total)) %>%
    dplyr::select(dplyr::all_of(race_cols))
  return(p_r_s_g)
}


#' Performs Bayesian Improved Surname Geocoding: Computes the probability a
#' person is of a specific racial group, conditioned on surname and geolocation.
#'
#' @param voter_file A tibble containing a list of voters (by row), and a
#'  column that denotes their surname.
#' @param surname_col A string denoting which column contains the voter surname.
#' @param geo_col A string denoting which column contains the geographic unit
#'  ID.
#' @param census_counts A tibble containing counts (divided amongst constituent
#'  groups) per geographic units (rows). If NULL, these counts will be obtained
#'  using the eiCompare helper function and the other parameters.
#' @param geography The geographic level at which to obtain Census data. If
#'  obtaining data from the decennial Census, can be up to "block". If
#'  obtaining data from the ACS, can only be up to "block group".
#' @param state The state from which to obtain Census data, as a string.
#' @param county The county(ies) from which to obtain Census data. If NULL,
#'  data is obtained from all counties in the state.
#' @param year The year to obtain Census data from. If 2010, uses decennial
#'  data. Otherwise, uses the 5-year ACS summary data.
#' @param geo_col_counts A string denoting the column in the counts tibble that
#'  refers to the geographic unit.
#' @param race_cols A list of strings denoting the columns containing racial
#'  groups.
#' @param impute_missing A bool denoting whether voter file entries that do not
#'  match at the geographic level should be imputed with either the surname
#'  probabilities, or should be imputed with probabilities calculated at a
#'  broader geographic unit.
#' @param verbose A boolean denoting the verbosity.
#' @param cache A boolean denoting whether Census data should be cached.
#' @return A tibble with rows denoting voters and columns denoting the
#'  probability that each voter is of a particular racial group.
#'
#' @importFrom dplyr bind_cols
#' @export bisg
bisg <- function(
  voter_file, surname_col, geo_col, census_counts = NULL, geography = NULL,
  state = NULL, county = NULL, year = NULL, geo_col_counts = "fips",
  race_cols = c("whi", "bla", "his", "asi", "oth"), impute_missing = TRUE,
  verbose = FALSE, cache = FALSE
) {
  # If counts aren't provided, obtain then via tidycensus
  if (is.null(census_counts)) {
    if (verbose) {
      message("Obtaining counts from Census Bureau using tidycensus.")
    }
    census_counts <- get_census_race_counts(
      geography = geography,
      state = state,
      county = county,
      year = year,
      cache = cache
    )
  }
  # Merge voter file with WRU surname database
  if (verbose) {
    message("Merging surnames.")
  }
  p_r_s <- compute_p_r_cond_s(
    voter_file = voter_file,
    surname_col = surname_col
  )
  # Compute the probability of race conditioned on geography and surname
  if (verbose) {
    message("Calculating BISG probabilities.")
  }
  p_r_s_g <- compute_p_r_cond_s_g(
    voter_file = voter_file,
    counts = census_counts,
    surname_col = surname_col,
    geo_col = geo_col,
    race_cols = race_cols,
    geo_col_counts = geo_col_counts,
    p_r_s = p_r_s
  )
  # If necessary, impute the records located in blocks recorded as having no
  # population
  if (impute_missing) {
    no_geocode_match <- is.na(p_r_s_g$whi)
    # Imputing is only necessary if an entry didn't match
    while (any(no_geocode_match)) {
      if (verbose) {
        message("Some voters didn't match to a geography.")
      }
      # Case 1: user did not provide a geography
      if (is.null(geography)) {
        if (verbose) {
          message(paste("Geography not provided.",
                        "Imputing with surname probabilities."))
        }
        p_r_s_g[no_geocode_match, race_cols] <- p_r_s[no_geocode_match,
                                                      race_cols]
      } else {
        # Case 2: user provides geography; extract new counts
        geography <- swap_census_geography(geography)
        if (verbose) {
          message(paste("Re-performing BISG at the", geography, "level."))
        }
        # Get Census counts at a broader spatial extent
        census_counts <- get_census_race_counts(
          geography = geography,
          state = state,
          county = county,
          year = year,
          cache = cache
        )
        # Re-perform BISG
        new_p_r_s_g <- compute_p_r_cond_s_g(
          voter_file = voter_file[no_geocode_match, ],
          counts = census_counts,
          surname_col = surname_col,
          geo_col = geo_col,
          race_cols = race_cols,
          geo_col_counts = geo_col_counts,
          p_r_s = p_r_s[no_geocode_match, ]
        )
        p_r_s_g[no_geocode_match, ] <- new_p_r_s_g
      }
      no_geocode_match <- is.na(p_r_s_g$whi)
    }
  }
  # Combine BISG probabilities back into voter file
  voter_file <- dplyr::bind_cols(voter_file, p_r_s_g)
  if (verbose) {
    message("BISG complete.")
  }
  return(voter_file)
}