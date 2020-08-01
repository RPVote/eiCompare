#' EI estimation for multiple races and candidates
#'
#' @param data A data.frame() object containing precinct-level turnout data by
#' race and candidate
#' @param cand_cols A character vector listing the column names for turnout for
#' each candidate
#' @param race_cols A character vector listing the column names for turnout by
#' race
#' @param totals_col The name of the column containing total votes cast in each
#' precinct
#' @param ntunes Integer number of pre-MCMC tuning runs, defaulted to 10
#' @param totaldraws Integer number of iterations per run in pre-MCMC tuning
#' runs, defaulted to 10000
#' @param samples Integer number of draws saved and used to compute estimates.
#' Total chain length is sample*thin + burnin
#' @param thin Integer specifying the thinning interval for posterior draws. Eg.
#' if thin = 2, every second draw gets added to the sample
#' @param burnin Integery specifying the number of initial iterations to be
#' discarded, defaulted to 10000
#' @param ci_size Numeric desired probability within the upper and lower
#' credible-interval bounds, defaulted to 0.95
#' @param ret_mcmc Boolean. If true, the full sample chains are returned
#'
#' @author Loren Collingwood
#' @author Ari Decter-Frain
#'
#' @references eiPack, King et al., (http://gking.harvard.edu/eiR)
#'
#' @value If ret_mcmc == TRUE, a list is returned containing results and a data
#' frame of the full chains from the MCMC. If ret_mcmc == FALSE, results are
#' returned in a dataframe
#'
#' @return A dataframe of ei results
ei_rxc <- function(
                   data,
                   cand_cols,
                   race_cols,
                   totals_col,
                   ntunes = 10,
                   totaldraws = 10000,
                   samples = 100000,
                   thin = 1,
                   burnin = 10000,
                   ci_size = 0.95,
                   ret_mcmc = FALSE) {

  # Check for valid arguments
  check_args(data, cand_cols, race_cols, totals_col)

  # Subset data
  data <- data[, c(cand_cols, race_cols, totals_col)]

  # Check for missings
  data <- remove_nas(data)

  # Get RxC formula object
  formula <- rxc_formula(cand_cols, race_cols)

  # Tune MCMC
  suppressWarnings(
    tune_nocov <- tuneMD(
      data = data,
      total = totals_col,
      formula = formula,
      ntunes = ntunes,
      totaldraws = totaldraws,
      ...
    )
  )

  # Bayes model estimation
  suppressWarnings(
    md_out <- ei.MD.bayes(
      formula = formula,
      sample = samples,
      data = data,
      total = totals_col,
      thin = thin,
      burnin = burnin,
      ret.mcmc = TRUE,
      tune.list = tune_nocov,
      ...
    )
  )

  # Extract district-level mcmc chains
  # These initially present raw population count estimates
  chains_raw <- md_out$draws$Cell.counts

  # Convert population estimates to proportions
  chains_pr <- matrix(NA, nrow = nrow(chains_raw), ncol = ncol(chains_raw))

  # Loop through races to get proportion of race voting for each cand
  for (i in 1:length(race_cols)) {
    race_indices <- grep(race_cols[i], colnames(chains_raw))
    race_draws <- chains_raw[, race_indices]
    race_pr <- race_draws / rowSums(race_draws)
    chains_pr[, race_indices] <- race_pr
  }

  # Get upper, lower CI limits
  ci_lower <- (1 - ci_size) / 2
  ci_upper <- 1 - ci_lower

  # Get point estimates and credible intervals
  results_table <- cbind(
    mcmcse::mcse.mat(chains_pr),
    mcmcse::mcse.q.mat(chains_pr, q = ci_lower)[, 1],
    mcmcse::mcse.q.mat(chains_pr, q = ci_upper)[, 1]
  )
  colnames(results_table) <- c("Mean", "SE", "CI_lower", "CI_upper")
  rownames(results_table) <- gsub("^.*?\\.", "", colnames(chains_raw))

  # Match expected output
  results_table <- get_md_bayes_gen_output(results_table, race_cols)

  # Return results and chains if requested
  if (ret_mcmc) {
    return(list(table = results_table, chains = chains_pr))
  } else {
    return(results_table)
  }
}
