#' Aggregates racial estimates across geographic units
#'
#' Obtains aggregated precinct counts of racial groups from a voter file. This
#' function is usually applied after application of BISG, when the voter file
#' has probabilistic estimates of race. However, it can be applied more
#' generally, aggregating actual counts of race.
#'
#' @param voter_file The voter file, as a dataframe. Should contain columns that
#'  denote the race probabilities or the actual race of the voter.
#' @param group_col A string denoting the column to aggregate over (e.g.,
#'  "precinct").
#' @param race_cols A list of strings denoting which columns contain
#'  probabilistic estimates of race. If aggregating over the ground truth race,
#'  this variable should be a single string denoting which column contains the
#'  race description.
#' @param race_keys A named list, with keys denoting the new race groups (e.g.,
#'  "white", "black", "hispanic", etc. The value of each key is a string or list
#'  of strings that denote which values in the race column map onto the key
#'  (e.g., mapping multiracial and Native American voters onto an "other" race
#'  category) This variable should only be provided if aggregating over the true
#'  race.
#' @param include_total A logical denoting whether the total counts (potentially
#'  rounded) should be included in the output dataframe.
#' @return Aggregated dataset of nrow() precinct size, including racial size
#'  precinct estimates. Dataset suitable for EI/RxC.
#'
#' @export precinct_agg_combine
#' @importFrom dplyr group_by_at summarise_at
precinct_agg_combine <- function(voter_file,
                                 group_col = "precinct",
                                 race_cols = NULL,
                                 race_keys = NULL,
                                 include_total = FALSE) {
  # If a race key is provided, assume that race_cols is ground truth
  if (!is.null(race_keys)) {
    if (is.null(race_cols)) {
      stop("Must specify a race column if providing ground truth race keys.")
    }
    for (race in names(race_keys)) {
      voter_file[[race]] <- as.numeric(
        voter_file[[race_cols]] %in% race_keys[[race]]
      )
    }
    # Overwrite race columns with new values
    race_cols <- names(race_keys)
  }

  # If no race columns are provided, assume BISG output
  if (is.null(race_cols)) {
    race_cols <- c("pred.whi", "pred.bla", "pred.his", "pred.asi", "pred.oth")
  }
  # Group voter file
  grouped_vf <- dplyr::group_by_at(voter_file, group_col)

  # Aggregate race columns across groups
  if (include_total) {
    funcs <- list("prop" = mean, "total" = function(x) round(sum(x)))
  } else {
    funcs <- list("prop" = mean)
  }
  agg <- dplyr::summarise_at(
    .tbl = grouped_vf,
    .funs = funcs,
    .vars = race_cols
  )

  return(agg)
}
