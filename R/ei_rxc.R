#' EI Bayesian simultaneous estimation for multiple races and candidates
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
#' @param seed A numeric seed value for replicating estimate results across
#' runs. If NULL, a random seed is chosen. Defaulted to NULL.
#' @param ret_mcmc Boolean. If true, the full sample chains are returned
#' @param verbose A boolean indicating whether to print out status messages.
#' @param ... Additional parameters passed to eiPack::tuneMD()
#'
#' @author Loren Collingwood <loren.collingwood@@ucr.edu>
#' @author Ari Decter-Frain <agd75@@cornell.edu>
#'
#' @references eiPack, King et al., (http://gking.harvard.edu/eiR)
#'
#' @return If ret_mcmc == TRUE, a list is returned containing results and a data
#' frame of the full chains from the MCMC. If ret_mcmc == FALSE, results are
#' returned in a dataframe
#'
#' @export
#'
#' @importFrom mcmcse mcse.mat mcse.q.mat
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
                   seed = NULL,
                   ret_mcmc = FALSE,
                   verbose = FALSE,
                   ...) {

  # Check for valid arguments
  check_args(data, cand_cols, race_cols, totals_col)

  # Subset data
  data <- data[, c(cand_cols, race_cols, totals_col)]

  # Check for missings
  data <- remove_nas(data)

  # Get RxC formula object
  formula <- rxc_formula(cand_cols, race_cols)

  # Set seed
  if (!is.null(seed)) {
    set.seed(seed)
  } else {
    seed <- sample(1:10e5, 1)
    if (verbose) {
      message(paste("Setting random seed equal to", seed, "..."))
    }
  }

  if (verbose) {
    message("Tuning parameters...")
  }
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

  if (verbose) {
    message("Collecting samples...")
  }
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

  # Extract district-level MCMC chains
  # These initially present raw population count estimates
  chains_raw <- md_out$draws$Cell.counts

  # Convert population estimates to proportions
  chains_pr <- matrix(NA, nrow = nrow(chains_raw), ncol = ncol(chains_raw))

  # Loop through races to get proportion of race voting for each cand
  # This loop is required to get proportions within races
  for (i in 1:length(race_cols)) {
    race_indices <- grep(race_cols[i], colnames(chains_raw))
    race_draws <- chains_raw[, race_indices]
    race_pr <- race_draws / rowSums(race_draws)
    chains_pr[, race_indices] <- race_pr
  }

  # Get upper, lower CI limits
  ci_lower <- (1 - ci_size) / 2
  ci_upper <- 1 - ci_lower

  if (verbose) {
    message(paste("Setting CI lower bound equal to", ci_lower))
    message(paste("Setting CI upper bound equal to", ci_upper))
  }

  # Get point estimates and credible interval bounds
  estimate <- mcmcse::mcse.mat(chains_pr)

  # The upper and lower CI estimates also have standard errors. Here these
  # errors are conservatively used to extend the 95% confidence bound further

  # look for function that just finds the 95% CI

  # Lower CI estimate
  lower <- mcmcse::mcse.q.mat(chains_pr, q = ci_lower)
  lower_est <- lower[, 1]
  lower_se <- lower[, 2]
  lower <- lower_est - lower_se

  # Upper CI estimate
  upper <- mcmcse::mcse.q.mat(chains_pr, q = ci_upper)
  upper_est <- upper[, 1]
  upper_se <- upper[, 2]
  upper <- upper_est + upper_se

  # This gets uses base R to get the correct candidate and race names from the
  # output of the chains
  cand_race_col <- gsub("^.*?\\.", "", colnames(chains_raw))
  cand_race_col <- unlist(strsplit(cand_race_col, "[.]", ))
  cand_col <- cand_race_col[seq(2, length(cand_race_col), 2)]
  race_col <- cand_race_col[seq(1, length(cand_race_col), 2)]

  # Create, name an output table
  results_table <- data.frame(cbind(estimate, lower, upper))
  results_table <- cbind(cand_col, race_col, results_table)
  colnames(results_table) <- c(
    "cand", "race", "mean", "se", "ci_lower", "ci_upper"
  )

  # Match expected output
  results_table <- get_md_bayes_gen_output(results_table, race_cols)

  # Return results and chains if requested
  if (ret_mcmc) {
    return(list(table = results_table, chains = chains_pr))
  } else {
    return(results_table)
  }
}
