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
#' @param name A unique identifier for the outputted eiCompare object.
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
#' @param eiCompare_class default = TRUE
#' @param seed A numeric seed value for replicating estimate results across
#' runs. If NULL, a random seed is chosen. Defaulted to NULL.
#' @param ret_mcmc Boolean. If true, the full sample chains are returned
#' @param verbose A boolean indicating whether to print out status messages.
#' @param diagnostic Boolean. If true, run diagnostic test to assess viability of MCMC
#' parameters (will return all chain results)
#' @param n_chains  Number of chains for diagnostic test. Default is set to 3.
#' @param plot_path A string to specify plot save location. If NULL, plot is not saved.
#' @param par_compute Boolean. If true, diagnostic test will be run in parallel.
#' @param n_cores The number of cores to use in parallel computation. Defaulted to NULL, in which case parallel::detectCores() - 1 is used
#' @param ... Additional parameters passed to eiPack::tuneMD()
#'
#' @author Loren Collingwood <loren.collingwood@@ucr.edu>, <loren.collingwood@@gmail.com>
#' @author Hikari Murayama <hikari_murayama@@berkeley.edu>
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
#' @importFrom doSNOW registerDoSNOW
#' @importFrom stats sd
#' @importFrom foreach getDoParWorkers %dopar% %do%
#' @importFrom parallel makeCluster stopCluster
#' @importFrom utils capture.output setTxtProgressBar
#' @importFrom coda as.mcmc mcmc.list gelman.plot
#'
#' @return A dataframe of ei results
ei_rxc <- function(
                   data,
                   cand_cols,
                   race_cols,
                   totals_col,
                   name = "",
                   ntunes = 10,
                   totaldraws = 10000,
                   samples = 100000,
                   thin = 1,
                   burnin = 10000,
                   ci_size = 0.95,
                   seed = NULL,
                   eiCompare_class = TRUE,
                   ret_mcmc = FALSE,
                   verbose = FALSE,
                   diagnostic = FALSE,
                   n_chains = 3,
                   plot_path = NULL,
                   par_compute = FALSE,
                   n_cores = NULL,
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

  # declare chain
  chain <- NULL

  if (verbose) {
    message("Collecting samples...")
  }

  if (diagnostic) {
    if (verbose) message("Running diagnostic")
    # Preparation for parallel processing if user specifies parallelization
    if (par_compute) {
      # Detect the number of cores you have
      parallel::detectCores()

      if (parallel::detectCores() < 4) {
        stop("It is not recommended to run parallel ei with less than 4 cores")
      } else if (parallel::detectCores() == 4) {
        warning("4 cores is the minimum recommended to run parallel ei")
      }
      if (verbose) message("Running in paralllel")

      # Standard to use 1 less core for clusters
      if (is.null(n_cores)) {
        clust <- parallel::makeCluster(parallel::detectCores() - 1)
      } else {
        clust <- parallel::makeCluster(n_cores)
      }

      # Register parallel processing cluster
      doSNOW::registerDoSNOW(clust)

      # Check to make sure that cores are set up correctly
      foreach::getDoParWorkers()
    }
    # Set infix option for parallel or not parallel
    `%myinfix%` <- ifelse(par_compute, `%dopar%`, `%do%`)

    # Init progressbar
    pb <- utils::txtProgressBar(
      min = 0,
      max = n_chains,
      style = 3
    )
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress = progress)

    md_mcmc <- foreach::foreach(
      chain = seq_len(n_chains),
      .inorder = FALSE,
      .packages = c("ei"),
      .options.snow = opts
    ) %myinfix% {
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
        race_indices <- grep(paste0('\\b',race_cols[i],'\\b'), colnames(chains_raw))
        race_draws <- chains_raw[, race_indices]
        race_pr <- race_draws / rowSums(race_draws)
        chains_pr[, race_indices] <- race_pr
      }

      setTxtProgressBar(pb, chain)

      # Make CODA object
      chains_pr <- coda::as.mcmc(chains_pr)
    }

    if (par_compute == TRUE) {
      # Stop clusters (always done between uses)
      parallel::stopCluster(clust)
      # Garbage collection (in case of leakage)
      gc()
    }
    # close progress bar
    close(pb)

    # Combine chains
    chains_list <- coda::mcmc.list(md_mcmc)

    if (!is.null(plot_path)) {
      if (verbose) message("Creating and saving plots")
      # Generate trace and general density plots
      pdf(paste0(plot_path, "trace_density.pdf"))
      plot(chains_list)
      grDevices::dev.off()
      
      # Generate Gelman plot for convergence
      pdf(paste0(plot_path, "gelman.pdf"))
      coda::gelman.plot(chains_list)
      grDevices::dev.off()
    }

    return(chains_list)
  } else {
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
      race_indices <- grep(paste0('\\b',race_cols[i],'\\b'), colnames(chains_raw))
      race_draws <- chains_raw[, race_indices]
      race_pr <- race_draws / rowSums(race_draws)
      chains_pr[, race_indices] <- race_pr
    }

    # Get point estimates and standard errors
    estimate <- mcmcse::mcse.mat(chains_pr)

    # Get standard deviation of each distribution
    sds <- apply(chains_pr, 2, stats::sd)

    # The upper and lower CI estimates also have standard errors. Here these
    # errors are conservatively used to extend the 95% confidence bound further

    # Set bounds according to
    if (eiCompare_class) {
      # eiCompare class object reports fixed CIs
      ci_lower <- 0.025
      ci_upper <- 0.975
    } else {
      # Get upper, lower CI limits
      ci_lower <- (1 - ci_size) / 2
      ci_upper <- 1 - ci_lower
      if (verbose) {
        message(paste("Setting CI lower bound equal to", ci_lower))
        message(paste("Setting CI upper bound equal to", ci_upper))
      }
    }

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

    # Get race and cand cols for the final table
    cand_col <- rep(cand_cols, each = length(race_cols))
    race_col <- rep(race_cols, times = length(cand_cols))

    # Put names on chains_pr
    names <- paste(cand_col, race_col, sep = "_")
    colnames(chains_pr) <- names

    # Create, name an output table
    results_table <- data.frame(cbind(estimate[, 1], sds, lower, upper))
    results_table <- cbind(cand_col, race_col, results_table)
    if (!eiCompare_class) {
      message(
        paste(
          "This results output is deprecated by the eiCompare class",
          "object. It will be removed in the near future."
        )
      )
      colnames(results_table) <- c(
        "cand", "race", "mean", "sd", "ci_lower", "ci_upper"
      )
    } else {
      colnames(results_table) <- c(
        "cand", "race", "mean", "sd", "ci_95_lower", "ci_95_upper"
      )
    }

    if (!eiCompare_class) {
      # Match expected output
      results_table <- get_md_bayes_gen_output(results_table)

      # Return results and chains if requested
      if (ret_mcmc) {
        return(list(table = results_table, chains = chains_pr))
      } else {
        return(results_table)
      }
    } else {
      output <- list(
        "type" = "RxC",
        "estimates" = results_table,
        "district_samples" = as.data.frame(chains_pr),
        "precinct_samples" = NULL,
        "stat_objects" = list(md_out),
        "name" = name
      )
      class(output) <- "eiCompare"
      return(output)
    }
  }
}
