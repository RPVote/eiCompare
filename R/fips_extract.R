#' Extract geographic unit codes from FIPS codes.
#'
#' This function will split up a column of FIPS codes into several columns, each
#' containing the individual code at different units. It is agnostic to the
#' level of the FIPS codes (i.e., FIPS codes are not required to be 15 digits
#' long). However, this function assumes that all FIPS codes begin at the state
#' level of precision.
#'
#' @param df The dataframe, with one column containing FIPS codes.
#' @param fips_col A string denoting the column containing the FIPS codes.
#' @param geo A string denoting the smallest geographic unit in the FIPS code.
#'  If NULL, the smallest geographic unit is determined based off the length of
#'  the FIPS codes.
#' @return A dataframe with additional columns containing the individual codes
#'  for different geographic units.
#'
#' @export fips_extract
#' @importFrom stringr str_length
#' @importFrom tidyr replace_na separate
fips_extract <- function(df, fips_col = NULL, geo = NULL) {
  # Store FIPS boundaries as variables
  state_idx <- 2
  county_idx <- 5
  tract_idx <- 11
  block_idx <- 15

  # If FIPS column is not provided, use the first one in dataframe
  if (is.null(fips_col)) {
    fips_col <- names(df)[1]
  }

  # Replace NA values with empty strings
  df[["fips_col_temp"]] <- tidyr::replace_na(df[[fips_col]], "")

  # If smallest geographic unit is not provided, determine from FIPS codes
  if (is.null(geo)) {
    n_characters <- max(stringr::str_length(df[["fips_col_temp"]]))
    # Determine the smallest geographic unit available in the FIPS codes
    if (n_characters <= state_idx) {
      stop("No splitting needed when FIPS codes are 2 characters or fewer.")
    } else if (n_characters == county_idx) {
      geo <- "county"
    } else if (n_characters == tract_idx) {
      geo <- "tract"
    } else if (n_characters == block_idx) {
      geo <- "block"
    } else {
      stop("Number of characters in FIPS code is incorrect.")
    }
  }

  # Create variable for separating FIPS codes
  if (geo == "county") {
    into <- c("state", "county")
    sep <- c(state_idx)
  } else if (geo == "tract") {
    into <- c("state", "county", "tract")
    sep <- c(state_idx, county_idx)
  } else if (geo == "block") {
    into <- c("state", "county", "tract", "block")
    sep <- c(state_idx, county_idx, tract_idx)
  } else {
    stop("FIPS codes can only be split at the county, tract, or block level.")
  }

  # Separate FIPS column into constituent units
  df <- tidyr::separate(
    data = df,
    col = "fips_col_temp",
    into = into,
    sep = sep
  )
  return(df)
}
