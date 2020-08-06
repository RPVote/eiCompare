#' Iterative EI Estimation
#'
#' This function runs enables running iterative ecoligical inference (EI) to
#' estimate the proportion of votes by different race/ethnicity groups for
#' different political candidates.
#'
#' Iterative EI iterates through all possible race-candidate pairs. For each
#' pair, votes by other races and for other candidates are binned and 2x2
#' ecological inference is run.
#'
#' @param data A data.frame() object containing precinct-level turnout data by
#' race and candidate
#' @param cand_cols A character vector listing the column names for turnout for
#' each candidate
#' @param race_cols A character vector listing the column names for turnout by
#' race
#' @param totals_col The name of the column containing total votes cast in each
#' precinct
#' @param erho A number passed directly to ei::ei(). Defaulted to 0.5
#' @param seed A numeric seed value for replicating estimate results across
#' runs. If NULL, a random seed is chosen. Defaulted to NULL.
#' @param plots A boolean indicating whether or not to include density and
#' tomography plots
#' @param betas A boolean to return precinct-level betas for each 2x2 ei
#' @param par_compute A boolean to conduct ei using parallel processing
#' @param verbose A boolean indicating whether to print out status messages.
#' @param ... Additional arguments passed directly to ei::ei()
#'
#' @importFrom doSNOW registerDoSNOW
#' @importFrom foreach foreach getDoParWorkers
#' @importFrom purrr lift
#' @importFrom utils capture.output setTxtProgressBar
#'
#' @author Loren Collingwood <loren.collingwood@@ucr.edu>
#' @author Ari Decter-Frain <agd75@@cornell.edu>
#'
#' @references eiPack. Gary King (1997). A Solution to the Ecological Inference
#' Problem. Princeton: Princeton University Press.
#'
#' @export
#'
#' @return dataframe of results from iterative ei
ei_iter <- function(
                    data,
                    cand_cols,
                    race_cols,
                    totals_col,
                    erho = 0.5,
                    seed = NULL,
                    plots = FALSE,
                    betas = FALSE,
                    par_compute = FALSE,
                    verbose = FALSE,
                    ...) {

  # Preparation for parallel processing if user specifies parallelization
  if (par_compute) {
    # Detect the number of cores you have
    parallel::detectCores()

    if (parallel::detectCores() < 4) {
      stop("It is not recommended to run parallel ei with less than 4 cores")
    } else if (parallel::detectCores() == 4) {
      warning("4 cores is the minimum recommended to run parallel ei")
    }

    # Standard to use 1 less core for clusters
    clust <- parallel::makeCluster(parallel::detectCores() - 1)

    # Register parallel processing cluster
    doSNOW::registerDoSNOW(clust)

    # Check to make sure that cores are set up correctly
    foreach::getDoParWorkers()
  }
  # Set infix option for parallel or not parallel
  `%myinfix%` <- ifelse(par_compute, `%dopar%`, `%do%`)

  # Check for valid arguments
  check_args(data, cand_cols, race_cols, totals_col)

  # Save any additional arguments to pass into ei inside foreach
  args_pass <- list(...)

  # Subset data
  data <- data[, c(cand_cols, race_cols, totals_col)]

  # Check for missings
  data <- remove_nas(data)

  # Force data to be a dataframe
  data <- as.data.frame(data)

  # Get race and cand lengths
  n_races <- length(race_cols)
  n_cands <- length(cand_cols)
  n_iters <- n_races * n_cands

  # Create race-cand pairs for iteration
  race_cand_pairs <- expand.grid(
    "race" = race_cols,
    "cand" = cand_cols,
    stringsAsFactors = FALSE
  )

  # Get seed if seed is null
  if (is.null(seed)) {
    seed <- sample(1:10e5, 1)
    if (verbose) {
      message(paste("Setting random seed equal to", seed))
    }
  }

  # Init progressbar
  pb <- utils::txtProgressBar(
    min = 0,
    max = n_iters,
    style = 3
  )
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)

  # Loop through each 2x2 ei
  ei_results <- foreach::foreach(
    i = 1:n_iters,
    .inorder = FALSE,
    .packages = c("ei", "stats", "utils"),
    .options.snow = opts
  ) %myinfix% {
    cand <- race_cand_pairs[i, "cand"]
    race <- race_cand_pairs[i, "race"]

    # Get formula
    formula <- stats::formula(paste(cand, "~", race), sep = " ")

    # Set seed
    set.seed(seed)

    # Run 2x2 ei
    utils::capture.output({
      ei_out <-
        suppressMessages(
          purrr::lift(ei::ei)(
            data = data,
            formula = formula,
            total = totals_col,
            erho = erho # ,
            # args_pass
          )
        )
    })

    # Plots to be added here
    if (plots) {
      do_nothing <- 3
    }

    # Extract mean, standard error for each precinct and district-wide
    res <- ei::eiread(
      ei.object = ei_out,
      "betab",
      "sbetab",
      "betaw",
      "sbetaw",
      "aggs",
      "maggs"
    )

    # get aggregate means
    betab_district_mean <- mean(res$aggs[1], na.rm = TRUE)
    betaw_district_mean <- mean(res$aggs[2], na.rm = TRUE)

    # Get aggregate ses
    # This works according to the aggregate formula in King, 1997, section 8.3
    aggs <- res$aggs
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

    # Put district-wide estimates in dataframe
    district_res <- data.frame(
      c(cand, "se"),
      c(betab_district_mean, ses[1]),
      c(betaw_district_mean, ses[2])
    )
    colnames(district_res) <- c("Candidate", race, "other")

    # Put precinct betas in dataframe
    precinct_res <- cbind(res$betab, res$betaw)
    colnames(precinct_res) <- paste(c("betab", "betaw"), cand, race, sep = "_")

    setTxtProgressBar(pb, i)

    list(district_res, precinct_res)
  }

  # close progress bar
  close(pb)

  # Separate out district level summary and precinct level results
  district_results <- sapply(ei_results, function(x) x[1])
  precinct_results <- sapply(ei_results, function(x) x[2])

  if (par_compute) {
    # Stop clusters (always done between uses)
    parallel::stopCluster(clust)
    # Garbage collection (in case of leakage)
    gc()
  }

  # Put results in dataframe
  results_table <- get_results_table(
    district_results,
    cand_col = cand_cols,
    race_col = race_cols,
    n_cand = n_cands,
    n_race = n_races,
    n_iter = n_iters
  )

  # If betas == TRUE, return a list with results plus df of betas
  if (betas) {
    df_betas <- betas_for_return(precinct_results, race_cand_pairs)
    to_return <- list(
      "race_group_table" = results_table,
      "all_betas" = df_betas
    )
    return(to_return)
  } else {
    return(results_table)
  }
}
