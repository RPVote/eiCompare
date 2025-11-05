#' Extract Precinct-Level Estimates from ei.MD.bayes Object
#'
#' Extracts precinct-specific ecological inference estimates from ei_rxc() output.
#' Uses exact string matching to handle variation column names
#'
#' @param eivote ei_rxc() output object containing stat_objects
#' @param cand_cols Character vector of candidate column names (e.g., c("pct_cand_A", "pct_cand_B"))
#' @param race_cols Character vector of race column names (e.g., c("pct_black", "pct_white"))
#' @param dat Original data frame used in ei_rxc() call
#' @param uniq Column name for precinct identifier (must exist in dat)
#'
#' @return Data frame with precinct IDs and race×candidate estimate columns
#'
#' @details
#' The function extracts md_out$draws$Beta from the ei_rxc() output, which contains
#' MCMC draws for each precinct-race-candidate combination. Beta column names follow
#' the format "beta.race_name.cand_name.precinct_idx". The function computes posterior
#' means across MCMC iterations for each precinct.
#'
#' Output columns follow expand.grid(cand, race) ordering, with column names formatted
#' as paste0(race, cand) (e.g., "pct_blackpct_cand_A").
#'
#' @examples
#' \donttest{
#' 
#' # library(eiCompare)
#' # data(gwinnett_ei)
#' #
#' # gwinnett_ei$precinct <- 1:nrow(gwinnett_ei)
#' #
#' # eivote <- ei_rxc( #this will take some time
#' #  data = gwinnett_ei,
#' # cand_cols = c("kemp", "abrams", "metz"),
#' # race_cols = c("white", "black", "other"),
#' #  totals_col = "turnout",
#' #  seed = 12345
#' #)
#'
#' # # Extract precinct-level estimates
#' # precinct_results <- extract_rxc_precinct(
#' #  eivote = eivote,
#' #  cand_cols = c("kemp", "abrams"),
#' #  race_cols = c("white", "black", "other"),
#' #  dat = gwinnett_ei,
#' #  uniq = "precinct"
#' #)
#'
#' #head(precinct_results)
#' }
#'
#' @export
extract_rxc_precinct <- function(eivote, cand_cols, race_cols, dat, uniq) {

  # Extract md_out object from ei_rxc wrapper
  eiMD_object <- eivote$stat_objects[[1]]

  # Extract Beta matrix (MCMC iterations × beta parameters)
  Beta <- eiMD_object$draws$Beta

  # Check that uniq column exists in dat
  if(!uniq %in% colnames(dat)) {
    stop(paste0("Column '", uniq, "' not found in dat. ",
                "Available columns: ", paste(colnames(dat), collapse = ", ")))
  }

  n_precincts <- nrow(dat)
  beta_colnames <- colnames(Beta)

  # Initialize result matrix (precincts × race-candidate combinations)
  result_matrix <- matrix(NA,
                          nrow = n_precincts,
                          ncol = length(race_cols) * length(cand_cols))

  # Loop through race-candidate combinations and extract precinct estimates
  col_idx <- 1
  for(race in race_cols) {
    for(cand in cand_cols) {

      # Build expected prefix pattern for exact matching
      # Format: beta.race.cand.precinct_number
      expected_prefix <- paste0("beta.", race, ".", cand, ".")

      # Find Beta columns matching this race-candidate pair
      matching_cols <- grep(paste0("^", gsub("\\.", "\\\\.", expected_prefix)),
                           beta_colnames,
                           value = FALSE)

      # Validation - should have exactly n_precincts matches
      if(length(matching_cols) != n_precincts) {
        stop(paste0("Column matching error for race='", race, "', cand='", cand,
                    "': found ", length(matching_cols), " columns but expected ",
                    n_precincts, " precincts"))
      }

      # Extract precinct indices and reorder to match dat row order
      precinct_nums <- sub(expected_prefix, "", beta_colnames[matching_cols])
      precinct_order <- order(as.numeric(precinct_nums))
      matching_cols_ordered <- matching_cols[precinct_order]

      # Calculate mean across MCMC iterations for each precinct
      result_matrix[, col_idx] <- colMeans(Beta[, matching_cols_ordered])
      col_idx <- col_idx + 1
    }
  }

  # Create column names (race + candidate, matching expand.grid order)
  col_names_df <- expand.grid(cand = cand_cols, race = race_cols)
  col_names <- paste0(col_names_df$race, col_names_df$cand)

  # Convert to data frame with column names
  result_df <- as.data.frame(result_matrix)
  colnames(result_df) <- col_names

  # Attach precinct IDs from original data as first column
  result_df <- cbind(dat[, uniq, drop = FALSE], result_df)

  return(result_df)
}
