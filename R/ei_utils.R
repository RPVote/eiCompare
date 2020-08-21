#' Remove missing values from dataset and return warning if any removed
#'
#' @author Ari Decter-Frain <agd75@@cornell.edu>
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
#' @author Ari Decter-Frain <agd75@@cornell.edu>
#' @param district_results A list of dataframes computed in the midst of ei_iter
#' @param cand_col Passed through from ei_iter
#' @param race_col Passed through from ei_iter
#' @param n_cand Passed through from ei_iter
#' @param n_race Passed through from ei_iter
#' @param n_iter Passed through from ei_iter
#' @param add_other A boolean. If true, adds an 'other' column to the output
#' when only one race group is included. Generally, set TRUE for ei_iter, FALSE
#' for ei_good.
#'
#' @return a dataframe of results that will work with table comparison funcs.
get_results_table <- function(
                              district_results,
                              cand_col,
                              race_col,
                              n_cand,
                              n_race,
                              n_iter,
                              add_other = TRUE) {
  # Build results matrix
  results_table <- matrix(nrow = (n_cand * 2 + 1), ncol = (n_race + 1))

  # If only one race column, add an "other" column to matrix
  single_race <- n_race == 1 & add_other
  if (single_race) results_table <- cbind(results_table, NA)

  # Add values from district_results list
  for (i in 1:n_iter) {
    cand <- district_results[[i]]$Candidate[1]
    race <- names(district_results[[i]])[2]

    mean_row <- match(cand, cand_col) + match(cand, cand_col) - 1
    sd_row <- mean_row + 1
    col <- match(race, race_col) + 1

    results_table[mean_row, col] <- district_results[[i]][1, 2] * 100
    results_table[sd_row, col] <- district_results[[i]][2, 2] * 100

    # Handle single race case
    if (single_race) {
      results_table[mean_row, (col + 1)] <- district_results[[i]][1, 3] * 100
      results_table[sd_row, (col + 1)] <- district_results[[i]][2, 3] * 100
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
  se_cols <- rep("se", length(cand_col))
  results_table[, 1] <- c(rbind(cand_col, se_cols), "Total")

  return(results_table)
}


#' Check for missing essential arguments from an ei function
#' @author Ari Decter-Frain <agd75@@cornell.edu>
#' @param data A dataframe upon which EI is to be performed
#' @param cand_cols A column of candidate names passed from ei functions
#' @param race_cols A column of race names passed from ei functions
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
      paste(missing, collapse = "\n")
    )
    stop(message)
  }
}

#' Manipulate precinct results to get betas as from ei_est_gen
#' @author Ari Decter-Frain <agd75@@cornell.edu>
#' @param precinct_results A list of betas from ei_iter()
#' @param race_cand_pairs The set of race/candidate pairs tested in ei_iter
betas_for_return <- function(precinct_results, race_cand_pairs) {
  betas <- data.frame(do.call(cbind, precinct_results))
  col_ids <- paste(race_cand_pairs$race, race_cand_pairs$cand, sep = "_")
  colnames(betas) <- paste(sub("\\..*", "", colnames(betas)), col_ids, sep = "_")
  return(betas)
}


#' Get 2x2 ei standard errors from ei object
#' Works according to the aggregate formula in King, 1997, section 8.3
#' @author Ari Decter-Frain <agd75@@cornell.edu>
#'
#' @param aggs A dataframe of aggregate value draws, taken from eiread()
get_ei_iter_se <- function(aggs) {
  ses <- c()
  for (i in 1:ncol(aggs)) {
    aggs_col <- aggs[, i]
    m <- mean(aggs_col)
    nsims <- length(aggs_col)
    devs <- m - aggs_col
    sq_devs <- devs^2
    sum_sq_devs <- sum(sq_devs)
    se <- sqrt(sum_sq_devs / nsims)
    ses <- append(ses, se)
  }
  return(ses)
}


#' Make rxc formula
#' @author Ari Decter-Frain <agd75@@cornell.edu>
#' @param cand_cols Character vector of candidate column names, passed from
#' ei_rxc
#' @param race_cols Character vector of candidate race names, passed from
#' ei_rxc
rxc_formula <- function(cand_cols, race_cols) {
  formula <- formula(
    paste(
      "cbind(",
      paste(cand_cols, collapse = ", "),
      ") ~ cbind(",
      paste(race_cols, collapse = ", "),
      ")",
      sep = ""
    )
  )
  return(formula)
}

#' Get md_bayes_gen() output from ei_rxc() output
#' @author Ari Decter-Frain <agd75@@cornell.edu>
#' @param results_table A results table from
#' @param tag A string added onto the columns names of each table. If empty
#'  string, no tag is added. Tags are separated by underscores.
#' @return A list of tables, each keyed by the racial group. The table contains
#'  the mean, standard error, and confidence bounds for the EI estimate.
get_md_bayes_gen_output <- function(results_table, tag = "") {
  races <- unique(results_table$race)

  # create list object output
  new_results <- list()

  for (i in seq_along(races)) {

    # get race name
    race <- races[i]

    # filter to where that race, then remove the race column
    race_res <- results_table[which(results_table$race == race), -2]

    # set rownames equal to the candidate column
    rownames(race_res) <- race_res$cand

    # remove the candidate column and multiple all numbers by 100
    race_res <- race_res[, -1] * 100

    # add tag to column names if necessary
    if (tag != "") {
      colnames(race_res) <- paste0(colnames(race_res), "_", tag)
    }

    # add to list
    new_results[[i]] <- race_res
  }
  names(new_results) <- races
  return(new_results)
}
