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
#' This function wraps around the ei function from the ei R package. This
#' function is unstable and can break in arbitrary ways. Errors
#' often emerge with particular values of the erho parameter. If the function
#' breaks, it will automatically try adjusting the erho parameter, first to 20,
#' then to 0.5.
#'
#' If problems persist, please submit an issue on the eiCompare github
#' repository and include the error message you receive.
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
#' @param erho A number passed directly to ei::ei(). Defaulted to 10. Can also
#' pass in a vector of erho values
#' @param seed An integer seed value for replicating estimate results across
#' runs. If NULL, a random seed is chosen. Defaulted to NULL.
#' @param plots A boolean indicating whether or not to include density and
#' tomography plots
#' @param eiCompare_class default = TRUE
#' @param betas A boolean to return precinct-level betas for each 2x2 ei
#' @param par_compute A boolean to conduct ei using parallel processing
#' @param n_cores The number of cores to use in parallel computation. Defaulted to NULL, in which case parallel::detectCores() - 1 is used
#' @param verbose A boolean indicating whether to print out status messages.
#' @param plot_path A string to specify plot save location. If NULL, plot is not saved
#' @param ... Additional arguments passed directly to ei::ei()
#'
#' @return If eiCompare_class = TRUE, an object of class eiCompare is returned.
#' Otherwise, a dataframe is returned that matches the formatting of ei_est_gen
#' output.
#'
#' @importFrom doSNOW registerDoSNOW
#' @importFrom foreach getDoParWorkers %dopar% %do%
#' @importFrom parallel makeCluster stopCluster
#' @importFrom bayestestR ci
#' @importFrom purrr lift
#' @importFrom utils capture.output setTxtProgressBar
#'
#' @author Loren Collingwood <loren.collingwood@@ucr.edu>
#' @author Ari Decter-Frain <agd75@@cornell.edu>
#' @author Hikari Murayama <hikari_murayama@@berkeley.edu>
#'
#' @references eiPack. Gary King (1997). A Solution to the Ecological Inference
#' Problem. Princeton: Princeton University Press.
#'
#' @export


ei_iter <- function(
                    data,
                    cand_cols,
                    race_cols,
                    totals_col,
                    name = "",
                    erho = 10,
                    seed = NULL,
                    plots = FALSE,
                    eiCompare_class = TRUE,
                    betas = FALSE,
                    par_compute = FALSE,
                    n_cores = NULL,
                    verbose = FALSE,
                    plot_path = NULL,
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

  if (verbose) {
    message(paste("Beginning", n_iters, "2x2 estimations..."))
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
    i = seq_len(n_iters),
    .inorder = FALSE,
    .packages = c("ei", "stats", "utils", "mvtnorm"),
    .options.snow = opts
  ) %myinfix% {
    cand <- race_cand_pairs[i, "cand"]
    race <- race_cand_pairs[i, "race"]

    # Get formula
    formula <- stats::formula(paste(cand, "~", race), sep = " ")

    # Set seed
    set.seed(seed)

    # Run 2x2 ei
    # This loop tries three different erho values before returning an error.
    # It first tries the provided erho value, then the default for ei (0.5),
    # then 20.
    # If a vector of erhos was entered, it will go through all of these before
    # failing
    n_erhos <- length(erho)
    if (n_erhos > 1) {
      erhos <- erho
    } else {
      erhos <- c(erho, 0.5, 20)
      n_erhos <- 3
    }
    ii <- 1
    while (ii < (n_erhos + 1)) {
      tryCatch(
        {
          utils::capture.output({
            ei_out <-
              suppressMessages(
                purrr::lift(ei::ei)(
                  data = data,
                  formula = formula,
                  total = totals_col,
                  erho = erhos[ii],
                  simulate = TRUE,
                  args_pass
                )
              )
          })
          break
          # This was meant to enable parameterization of the ei importance sample
          # size, but its inclusion changes results dramatically.
          # utils::capture.output({
          #  ei_out <- suppressMessages(ei_sim(ei_out, samples))
          # })
        },
        error = function(cond) {
          if (ii == n_erhos) {
            stop(
              message(
                paste(
                  "\n", format(formula), "iteration failed", n_erhos,
                  "times.\nType ?ei_iter for guidance on how to proceed.\n",
                  "Error on final failure:\n", cond
                )
              )
            )
          } else {
            message(
              paste(
                "\n",
                format(formula),
                "iteration failed. Retrying with erho =",
                as.character(erhos[ii]),
                "..."
              )
            )
          }
        }
      )
      # Advance iteration
      ii <- ii + 1
    }

    if (plots & !is.null(plot_path)) {
      # Create tomography plots
      grDevices::png(paste0(plot_path, "tomography_", cand, "_", race, ".png"),
        units = "in", height = 6, width = 6, res = 500
      )
      plot(ei_out, "tomogE")
      graphics::mtext(paste(cand, race, sep = " "),
        outer = T, line = -1
      )

      grDevices::dev.off()

      # Create denity plots
      grDevices::png(paste0(plot_path, "density_", cand, "_", race, ".png"),
        units = "in", height = 6, width = 6, res = 500
      )
      plot(ei_out, "betab", "betaw")
      graphics::mtext(paste(cand, race, sep = " "),
        outer = T, line = -1
      )
      grDevices::dev.off()
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
    betab_district_mean <- mean(res$aggs[, 1], na.rm = TRUE)
    betaw_district_mean <- mean(res$aggs[, 2], na.rm = TRUE)

    # Get aggregate ses
    # This works according to the aggregate formula in King, 1997, section 8.3
    aggs <- res$aggs
    ses <- c()
    for (k in 1:ncol(aggs)) {
      aggs_col <- aggs[, k]
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

    # Save out aggs
    aggs_b <- eiread(ei_out, "aggs")

    setTxtProgressBar(pb, i)

    list(district_res, precinct_res, aggs_b, list(race, cand, ei_out))
  }

  # Stop clusters as soon as done parallel processing
  if (par_compute) {
    # Stop clusters (always done between uses)
    parallel::stopCluster(clust)
    # Garbage collection (in case of leakage)
    gc()
  }

  # close progress bar
  close(pb)

  # Extract results objects
  district_results <- sapply(ei_results, function(x) x[1])
  precinct_results <- sapply(ei_results, function(x) x[2])
  agg_results <- sapply(ei_results, function(x) x[3])
  ei_objects <- sapply(ei_results, function(x) x[4])


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
    message("Creating density plots")

    # Combine aggregate results for district level values into one data frame
    agg_race <- sapply(ei_results, function(x) colnames(x[[1]])[2])
    agg_cand <- sapply(ei_results, function(x) x[[1]]$Candidate[1])
    agg_race_cand <- paste0(agg_race, "_", agg_cand)
    agg_race_cand_2 <- rep(agg_race_cand, each = 2)

    agg_colnames <- as.vector(sapply(agg_results, function(x) colnames(x)))
    new_colnames <- paste(tolower(agg_colnames), agg_race_cand_2, sep = "_")

    agg_betas <- data.frame(do.call(cbind, agg_results))
    colnames(agg_betas) <- new_colnames

    # Create density plots
    density_plots <- overlay_density_plot(
      agg_betas,
      results_table,
      race_cols,
      cand_cols,
      plot_path,
      ei_type = "ei"
    )

    # Create degree of racially polarized voting
    rpv_distribution <- rpv_density(agg_betas, plot_path)
  }

  if (eiCompare_class) {

    # Set up containers
    races <- c()
    cands <- c()
    means <- c()
    ses <- c()
    sds <- c()
    ci_lowers <- c()
    ci_uppers <- c()

    district_samples <- as.data.frame(matrix(ncol = 0, nrow = 99))
    precinct_samples <- list()

    for (i in 1:length(ei_objects)) {
      ei_object <- ei_objects[[i]][[3]]
      cand <- ei_objects[[i]][[2]]
      race <- ei_objects[[i]][[1]]
      cands <- append(cands, cand)
      races <- append(races, race)

      # Get estimates
      aggs <- ei::eiread(ei_object, "aggs")[, 1]

      # Both CIs
      suppressMessages({
        suppressWarnings({
          cis <- bayestestR::ci(aggs, ci = 0.95, method = "HDI")
        })
      })
      ci_lowers <- append(ci_lowers, cis$CI_low)
      ci_uppers <- append(ci_uppers, cis$CI_high)

      # Mean
      mean <- mean(aggs, na.rm = TRUE)
      means <- append(means, mean)

      # Standard deviation
      sd <- sd(aggs, na.rm = TRUE)
      sds <- append(sds, sd)

      # Add district chains to dataframe
      district_name <- paste(cand, race, sep = "_")
      district_samples[[district_name]] <- as.numeric(aggs)

      # Get samples of precinct-level estimates
      prec_res <- ei::eiread(
        ei.object = ei_object,
        "betab",
        "sbetab",
        "betaw",
        "sbetaw"
      )

      # Put precinct betas in dataframe
      precinct_res <- cbind(
        prec_res$betab,
        prec_res$sbetab,
        prec_res$betaw,
        prec_res$sbetaw
      )
      colnames(precinct_res) <- c("betab", "sbetab", "betaw", "sbetaw")

      precinct_samples[[district_name]] <- precinct_res
    }

    # Make estimates table
    estimates <- data.frame(cbind(means, sds, ci_lowers, ci_uppers))
    estimates <- cbind(cands, races, estimates)
    colnames(estimates) <- c(
      "cand", "race", "mean", "sd", "ci_95_lower", "ci_95_upper"
    )

    output <- list(
      "type" = "iter",
      "estimates" = estimates,
      "district_samples" = district_samples,
      "precinct_samples" = precinct_samples,
      "stat_objects" = ei_objects,
      "name" = name
    )
    class(output) <- "eiCompare"
    return(output)
  } else {
    message(
      paste(
        "This results output is deprecated by the eiCompare class",
        "object. It will be removed in the near future."
      )
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
}
