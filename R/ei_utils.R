#' Remove missing values from dataset and return warning if any removed
remove_nas <- function(data) {
  out <- na.omit(data)
  rows_w_na <- nrow(data) - nrow(out)
  if (rows_w_na != 0) {
    warning(paste("Removing", rows_w_na, "row(s) with missing values from the dataset.\nConsider running preprocessing functions before running EI."))
  }
  return(out)
}

#' Get results dataframe from a list of results as from ei_est_gen
get_results_table <- function(
                              district_results,
                              cand_col = cand_cols,
                              race_col = race_cols,
                              n_cand = n_cands,
                              n_race = n_races,
                              n_iter = n_iters) {
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
  totals <- sapply(1:totals_num, function(x) sum(results_table[seq(1, n_cand * 2 - 1, 2), x + 1]))
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
check_args <- function(data,
                       cand_cols,
                       race_cols,
                       totals_col,
                       totals_null = TRUE) {
  cols <- names(data)
  missing_cands <- cand_cols[which(!(cand_cols %in% cols))]
  missing_races <- race_cols[which(!(race_cols %in% cols))]
  missing_total <- totals_col[which(!(totals_col %in% cols))]

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
betas_for_return <- function(precinct_results, race_cand_pairs) {
  betas <- do.call(cbind, precinct_results)
  col_ids <- paste(race_cand_pairs$race, race_cand_pairs$cand, sep = "_")
  colnames(betas) <- paste(colnames(betas), col_ids, sep = "_")
  return(betas)
}
