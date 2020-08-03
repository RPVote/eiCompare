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
#' @param erho A number passed directly to ei::ei(). Defaulted to 10
#' @param sample The number of samples used in ei estimation. Defaulted to 1000
#' @param plots A boolean indicating whether or not to include density and
#' tomography plots
#' @param betas A boolean to return precinct-level betas for each 2x2 ei
#' @param par_compute A boolean to conduct ei using parallel processing
#' @param plot_path A string to specify plot save location. Defaulted to working directory
#' @param ... Additional arguments passed directly to ei::ei()
#'
#' @importFrom doSNOW registerDoSNOW
#' @importFrom foreach foreach getDoParWorkers
#' @importFrom purrr lift
#' @importFrom utils capture.output setTxtProgressBar
#'
#' @author Loren Collingwood <loren.collingwood@@ucr.edu>
#' @author Ari Decter-Frain
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
                    erho = 10,
                    sample = 1000,
                    plots = FALSE,
                    betas = FALSE,
                    par_compute = FALSE,
                    plot_path = "",
                    ...) {

  # Preparation for parallel processing if user specifies parallelization
  if (par_compute == TRUE) {
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

    # Run 2x2 ei
    # ADD TRY CATCH SO THAT IF PARALLELIZATION=TRUE THEN KILL CLUSTERS
    utils::capture.output({
      ei_out <-
        suppressMessages(
          purrr::lift(ei::ei)(
            data = data,
            formula = formula,
            total = totals_col,
            erho = erho,
            sample = sample,
            args_pass
          )
        )
    })

    # # Plots to be added here
    # if (plots) {
    #   do_nothing <- 3
    # }

    # Extract mean, standard error for each precinct
    precinct_res <- ei::eiread(
      ei.object = ei_out,
      "betab",
      "sbetab",
      "betaw",
      "sbetaw"
    )

    # Extract district-wide means, sds
    beta_b_mean <- summary(ei_out)[[10]][1, 1]
    beta_w_mean <- summary(ei_out)[[10]][2, 1]
    beta_b_sd <- summary(ei_out)[[10]][1, 2]
    beta_w_sd <- summary(ei_out)[[10]][2, 2]

    # Put district-wide estimates in dataframe
    district_res <- data.frame(
      c(cand, "se"),
      c(beta_b_mean, beta_b_sd),
      c(beta_w_mean, beta_w_sd)
    )
    colnames(district_res) <- c("Candidate", race, "other")

    # Put precinct betas in dataframe
    precinct_res <- cbind(precinct_res[[1]], precinct_res[[3]])
    colnames(precinct_res) <- c("betab", "betaw")

    setTxtProgressBar(pb, i)

    list(district_res, precinct_res)
  }

  # close progress bar
  close(pb)

  # Separate out district level summary and precinct level results
  district_results <- sapply(ei_results, function(x) x[1])
  precinct_results <- sapply(ei_results, function(x) x[2])

  if (par_compute == TRUE) {
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

  # Plots moved to here
  # Density plots
  if (plots) {
    print("Creating density plots")
    density_plots <- overlay_density_plot(betas_ei, plot_path, ei_type = "ei")
  }


  # If betas == TRUE, return a list with results plus df of betas
  if (betas == TRUE) {
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
