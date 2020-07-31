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
#' @param seed A numeric seed value for replicating estimate results across runs
#' . Defaulted to NULL.
#' @param plots A boolean indicating whether or not to include density and
#' tomography plots
#' @param betas A boolean to return precinct-level betas for each 2x2 ei
#' @param ... Additional arguments passed directly to ei::ei()
#'
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
                    seed = NULL,
                    plots = FALSE,
                    betas = FALSE,
                    ...) {

  # Check for valid arguments
  check_args(data, cand_cols, race_cols, totals_col)

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

  # Create lists for storing loop results
  district_results <- list()
  precinct_results <- list()

  # Loop through each 2x2 ei
  for (i in 1:n_iters) {
    cand <- race_cand_pairs[i, "cand"]
    race <- race_cand_pairs[i, "race"]

    # Get formula
    formula <- stats::formula(paste(cand, "~", race), sep = " ")

    # Set seed
    if (!is.null(seed)) {
      set.seed(seed)
    }

    # Run 2x2 ei
    capture.output({
      ei_out <-
        suppressMessages(
          ei::ei(
            data = data,
            formula = formula,
            total = totals_col,
            erho = erho,
            sample = sample,
            ...
          )
        )
    })

    # Plots to be added here
    if (plots) {
      do_nothing <- 3
    }

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

    # Store both in list
    district_results <- append(district_results, list(district_res))
    precinct_results <- append(precinct_results, list(precinct_res))

    setTxtProgressBar(pb, i)
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

  # If betas == T, return a list with results plus df of betas
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
