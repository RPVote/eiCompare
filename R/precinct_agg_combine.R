#' Aggregates racial estimates across geographic units
#'
#' Obtains aggregated precinct counts of racial groups from a voter file. This
#' function is usually applied after application of BISG, when the voter file
#' has probabilistic estimates of race. However, it can be applied more
#' generally, aggregating actual counts of race. This function can perform
#' aggregation over probabilistic estimates of race and ground truth race at the
#' same time.
#'
#' @param voter_file The voter file, as a dataframe. Should contain columns that
#'  denote the race probabilities or the actual race of the voter.
#' @param group_col A string denoting the column to aggregate over (e.g.,
#'  "precinct").
#' @param race_cols A list of strings denoting which columns contain
#'  probabilistic estimates of race. By default, it assumes output from WRU.
#'  This function does not require all WRU output columns be present in the
#'  voter file; rather, it checks which outputs are present and uses those in
#'  aggregation.
#' @param true_race_col A string denoting which (single) column in the voter
#'  file specifies the true race of the voter. If this variable is provided,
#'  then true_race_keys must also be provided, or an error is thrown.
#' @param true_race_keys A named list, with keys denoting the new race groups
#'  (e.g., "white", "black", "hispanic", etc.). The value of each key is a
#'  string or list of strings that denote which columns in the voter file map
#'  onto the new ground truth race column. This is useful, for example, in
#'  mapping multiracial and Native American voters onto an "other" race
#'  category. This variable should only be provided if aggregating over the true
#'  race.
#' @param include_total A logical denoting whether the total counts (potentially
#'  rounded) should be included in the output dataframe.
#' @return Aggregated dataset of nrow() precinct size, including racial size
#'  precinct estimates. Dataset suitable for EI/RxC.
#'
#' @examples
#' # Create synthetic voter file with typical BISG output
#' voter_file <- data.frame(
#'   precinct = c(1, 1, 2, 2),
#'   pred.whi = c(0.10, 0.20, 0.30, 0.40),
#'   pred.bla = c(0.40, 0.30, 0.20, 0.10),
#'   pred.his = c(0.10, 0.20, 0.30, 0.40),
#'   pred.asi = c(0.30, 0.20, 0.10, 0.00),
#'   pred.oth = c(0.10, 0.10, 0.10, 0.10)
#' )
#' # Function uses these column names by default
#' agg <- precinct_agg_combine(
#'   voter_file = voter_file,
#'   group_col = "precinct",
#'   include_total = FALSE
#' )
#'
#' # Running aggregation with a ground truth race column
#' voter_file <- data.frame(
#'   precinct = c(1, 1, 1, 1, 2, 2, 2, 2),
#'   race = c("BL", "WH", "NA", "MR", "BL", "WH", "BL", "BL")
#' )
#' # Need to specify race keys for true race column
#' agg <- precinct_agg_combine(
#'   voter_file = voter_file,
#'   group_col = "precinct",
#'   true_race_col = "race",
#'   true_race_keys = list("whi" = "WH", "bla" = "BL", "oth" = c("NA", "MR")),
#'   include_total = TRUE
#' )
#'
#' # Running aggregation for both predicted and true race columns. Note the
#' # change in column names, which means we need to specify column names.
#' voter_file <- data.frame(
#'   precinct = c(1, 1, 2, 2),
#'   p.whi = c(0.10, 0.20, 0.30, 0.40),
#'   p.bla = c(0.40, 0.30, 0.20, 0.10),
#'   p.his = c(0.10, 0.20, 0.30, 0.40),
#'   p.asi = c(0.30, 0.20, 0.10, 0.00),
#'   p.oth = c(0.10, 0.10, 0.10, 0.10),
#'   race = c("BL", "WH", "BL", "WH")
#' )
#' agg <- precinct_agg_combine(
#'   voter_file = voter_file,
#'   group_col = "precinct",
#'   race_cols = c("p.whi", "p.bla", "p.his", "p.asi", "p.oth"),
#'   true_race_col = "race",
#'   true_race_keys = list("whi" = "WH", "bla" = "BL"),
#'   include_total = FALSE
#' )
#' @export precinct_agg_combine
#' @importFrom dplyr group_by_at summarise_at
precinct_agg_combine <- function(voter_file,
                                 group_col = "precinct",
                                 race_cols = NULL,
                                 true_race_col = NULL,
                                 true_race_keys = NULL,
                                 include_total = FALSE) {
  # If no race columns are provided, assume BISG output
  if (is.null(race_cols)) {
    # BISG output columns
    race_cols <- c("pred.whi", "pred.bla", "pred.his", "pred.asi", "pred.oth")
    # Select columns present in voter file
    race_cols <- race_cols[race_cols %in% names(voter_file)]
    # Check if no columns are present
    if (length(race_cols) == 0) {
      race_cols <- c()
    }
  }

  # If a race key is provided, assume that race_cols is ground truth
  if (!is.null(true_race_keys)) {
    if (is.null(true_race_col)) {
      stop("Must specify a race column if providing ground truth race keys.")
    }
    for (race in names(true_race_keys)) {
      voter_file[[race]] <- as.numeric(
        voter_file[[true_race_col]] %in% true_race_keys[[race]]
      )
    }
    # Overwrite race columns with new values
    race_cols <- c(race_cols, names(true_race_keys))
  }

  # If no columns are selected for aggregation, stop
  if (length(race_cols) == 0) {
    stop("No columns selected for aggregation.")
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
