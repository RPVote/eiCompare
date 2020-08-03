#' Remove missing values from dataset and return warning if any removed
#' @param data A dataframe on which ei is to be performed.
remove_nas <- function(data) {
  out <- na.omit(data)
  rows_w_na <- nrow(data) - nrow(out)
  if (rows_w_na != 0) {
    warning(paste("Removing", rows_w_na, "row(s) with missing values from the dataset.\nConsider running preprocessing functions before running EI."))
  }
  return(out)
}

#' Get results dataframe from a list of results as from ei_est_gen
#' @param district_results A list of dataframes computed in the midst of ei_iter
#' @param cand_col Passed through from ei_iter
#' @param race_col Passed through from ei_iter
#' @param n_cand Passed through from ei_iter
#' @param n_race Passed through from ei_iter
#' @param n_iter Passed through from ei_iter
#'
#' @return a dataframe of results that will work with table comparison funcs.
get_results_table <- function(
                              district_results,
                              cand_col,
                              race_col,
                              n_cand,
                              n_race,
                              n_iter) {
  # Build results matrix
  results_table <- matrix(nrow = (n_cand * 2 + 1), ncol = (n_race + 1))

  # If only one race column, add an "other" column to matrix
  single_race <- length(n_race) == 1
  if (single_race) results_table <- cbind(results_table, NA)

  # Add values from district_results list
  for (i in 1:n_iter) {
    cand <- district_results[[i]]$Candidate[1]
    race <- names(district_results[[i]])[2]

    mean_row <- match(cand, cand_col) + match(cand, cand_col) - 1
    sd_row <- mean_row + 1
    col <- match(race, race_col) + 1

    results_table[mean_row, col] <- district_results[[i]][1, 2]
    results_table[sd_row, col] <- district_results[[i]][2, 2]

    # Handle single race case
    if (single_race) {
      results_table[mean_row, (col + 1)] <- district_results[[i]][1, 3]
      results_table[sd_row, (col + 1)] <- district_results[[i]][2, 3]
    }
  }

  # Add totals. Totals num ensures single race case is handled
  totals_num <- ncol(results_table) - 1
  totals <- sapply(1:totals_num, function(x) {
    sum(results_table[seq(1, n_cand * 2 - 1, 2), x + 1])
  })
  results_table[nrow(results_table), 2:ncol(results_table)] <- totals

  # Add column names
  results_table <- as.data.frame(results_table)
  if (single_race) {
    names(results_table) <- c("Candidate", race_col, "other")
  } else {
    names(results_table) <- c("Candidate", race_col)
  }

  # Add first column of candidate info
  seq_split <- 2:n_cand
  results_table[, 1] <-
    c(
      R.utils::insert(
        cand_col,
        ats = seq_split,
        values = rep("sd", n_cand - 1)
      ),
      "sd",
      "Total"
    )
  return(results_table)
}


#' Check for missing essential arguments from an ei function
#' @param data A dataframe upon which EI is to be performed
#' @param cand_cols A column of candidate names passed from ei functions
#' @param race_rols A column of race names passed from ei functions
#' @param totals_col The name of a column passed from ei functions
#' @param totals_null A boolean. If TRUE, ignore totals_col argument
check_args <- function(data,
                       cand_cols,
                       race_cols,
                       totals_col,
                       totals_null = FALSE) {
  cols <- names(data)
  missing_cands <- cand_cols[which(!(cand_cols %in% cols))]
  missing_races <- race_cols[which(!(race_cols %in% cols))]
  if (!totals_null) {
    missing_total <- totals_col[which(!(totals_col %in% cols))]
  }

  missing <- c(missing_cands, missing_races, missing_total)

  if (length(missing) > 0) {
    message <- paste(
      "The following specified columns are not in the dataset:",
      missing
    )
    stop(message)
  }
}

#' Manipulate precinct results to get betas as from ei_est_gen
#' @param precinct_results A list of betas from ei_iter()
#' @param race_cand_pairs The set of race/candidate pairs tested in ei_iter
betas_for_return <- function(precinct_results, race_cand_pairs) {
  betas <- do.call(cbind, precinct_results)
  col_ids <- paste(race_cand_pairs$race, race_cand_pairs$cand, sep = "_")
  colnames(betas) <- paste(colnames(betas), col_ids, sep = "_")
  return(betas)
}
