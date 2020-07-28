#' Iterative EI Estimation
#'
#' @param data A data.frame() object containing precinct-level turnout data by race and candidate
#' @param cand_cols A character vector listing the column names for turnout for each candidate
#' @param race_cols A character vector listing the column names for turnout by race
#' @param totals_col The name of the column containing total votes cast in each precinct
#' @param rho A number passed directly to ei::ei(). Defaulted to 10
#' @param sample The number of samples used in ei estimation. Defaulted to 1000
#' @param plots A boolean indicating whether or not to include density and tomography plots
#' @param betas A boolean to return precinct-level betas for each 2x2 ei
#' @param ... Additional arguments passed directly to ei::ei()
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
                    ...) {
  # subset data
  data <- data[, c(cand_cols, race_cols, totals_col)]

  # check for missings
  data <- remove_nas(data)

  # get race and cand lengths
  n_races <- length(race_cols)
  n_cands <- length(cand_cols)
  n_iters <- n_races * n_cands

  # create 2x2 ei formulas
  race_cand_pairs <- expand.grid(
    "race" = race_cols,
    "cand" = cand_cols,
    stringsAsFactors = FALSE
  )

  # make loop output containers
  race_group_table <- list()
  beta_full_hold <- list()

  # init progressbar
  pb <- utils::txtProgressBar(
    min = 0,
    max = n_iters,
    style = 3
  )

  # create lists for storing loop results
  district_results <- list()
  precinct_results <- list()

  # loop through each 2x2 ei
  for (i in 1:n_iters) {
    cand <- race_cand_pairs[i, "cand"]
    race <- race_cand_pairs[i, "race"]

    # get formula
    formula <- stats::formula(paste(cand, "~", race), sep = " ")

    # run 2x2 ei
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

    # plots to be added here
    if (plots) {
      do_nothing <- 3
    }

    # extract mean, standard error for each precinct
    precinct_res <- ei::eiread(
      ei.object = ei_out,
      "betab",
      "sbetab",
      "betaw",
      "sbetaw"
    )

    # extract district-wide means, sds
    beta_b_mean <- summary(ei_out)[[10]][1, 1]
    beta_w_mean <- summary(ei_out)[[10]][2, 1]
    beta_b_sd <- summary(ei_out)[[10]][1, 2]
    beta_w_sd <- summary(ei_out)[[10]][2, 2]

    # put district-wide estimates in dataframe
    district_res <- data.frame(
      c(cand, "se"),
      c(beta_b_mean, beta_b_sd) # ,
      # c(beta_w_mean, beta_w_sd),
    )
    colnames(district_res) <- c("Candidate", race) # , 'other')

    # put precinct betas in dataframe
    precinct_res <- cbind(precinct_res[[1]], precinct_res[[3]])
    colnames(precinct_res) <- c("betab", "betaw")

    # store both in list
    district_results <- append(district_results, list(district_res))
    precinct_results <- append(precinct_results, list(precinct_res))

    setTxtProgressBar(pb, i)
  }

  # put results in dataframe
  results_table <- get_results_table(district_results,
    cand_col = cand_cols,
    race_col = race_cols,
    n_cand = n_cands,
    n_race = n_races,
    n_iter = n_iters
  )
  return(results_table)
}
