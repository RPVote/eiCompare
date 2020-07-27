#' MD Bayes Generalize
#'
#' Tunes and estimates MD Bayes algorithm (ei.MD.bayes). This, combined with
#' md_bayes_table() produces tables of results compatible with EI table of
#' results.
#'
#'
#' @param data A data.frame object containing turnout information by
#' candidate and race
#' @param cand_cols A character vector of the names of columns containing
#' data on turnout for each candidate
#' @param race_cols A character vector of the names of columns containing
#' data on turnout by race group.
#' @param totals_col The name of the column containing vote totals for each
#' row. If NULL, assumes columns are totals, not pct.
#' @param ntunes Numeric. How much to tune tuneMD. Default = 10
#' @param totaldraws Numeric. Number of total draws from MD. Default = 10000
#' @param seed Numeric. Default = 12345
#' @param sample Numeric. Default = 10000
#' @param thin Numeric. Default = 10
#' @param burnin Numeric. Default = 10000
#' @param ret_mcmc Logical. Default = TRUE
#' @param ci numeric vector of credible interval (low/high), default is 95
#' percent= c(0.025, 0.975)
#' @param ci_true Logical, default = TRUE. Include credible intervals in
#' reported results.
#' @param produce_draws Logical, default is FALSE. Produces two-item list of
#' table and md.bayes() mcmc draws (for additional testing and analysis)
#' @param verbose A logical to print messages, ei summaries as iterations progress.
#' @param ... Additional arguments passed to tuneMD() and ei.MD.bayes()
#' @return List object of length 1 (when produce_draws=FALSE). List object of
#' length 2 (when produce_draws=TRUE). First item is list of race x candidate
#' tabular results, with mean, SE, and credible intervals. Second item is mcmc
#' draws.
#'
#' @author Loren Collingwood <loren.collingwood@@ucr.edu>
#' @references eiPack, King et. al. (http://gking.harvard.edu/eiR)
#' @examples
#'
#'
#' # TOY DATA EXAMPLE
#' canda <- c(.1, .09, .85, .9, .92)
#' candb <- 1 - canda
#' white <- c(.8, .9, .10, .08, .11)
#' black <- 1 - white
#' total <- c(30, 80, 70, 20, 29)
#' toy <- data.frame(canda, candb, white, black, total)
#'
#' # only use one group for example
#' cand_cols <- c("canda", "candb")
#' race_cols <- c("black", "white")
#'
#' # Then execute md_bayes_gen(); not run here due to time
#' md_bayes_gen(
#'   data = toy,
#'   cand_cols = cand_cols,
#'   race_cols = race_cols,
#'   totals_col = "total",
#'   ntunes = 1,
#'   thin = 1,
#'   totaldraws = 100,
#'   sample = 10,
#'   burnin = 1
#' )
#'
#' # Add in mcmc drawings
#' drawings <- md_bayes_gen(
#'   dat = toy,
#'   form = form,
#'   total_yes = FALSE,
#'   ntunes = 1,
#'   thin = 1,
#'   totaldraws = 100,
#'   sample = 10,
#'   burnin = 1,
#'   produce_draws = TRUE
#' )
#' head(drawings$draws)
#' @importFrom stats as.formula
#' @importFrom stringr str_squish
#' @importFrom mcmcse mcse.mat mcse.q.mat
#' @export md_bayes_gen
md_bayes_gen <- function(
                         data,
                         cand_cols,
                         race_cols,
                         totals_col = NULL,
                         ntunes = 10,
                         totaldraws = 10000,
                         seed = 12345,
                         sample = 1000,
                         thin = 100,
                         burnin = 10000,
                         ret_mcmc = TRUE,
                         ci = c(0.025, 0.975),
                         ci_true = TRUE,
                         produce_draws = FALSE,
                         verbose = FALSE,
                         ...) {
  # get formula
  formula <- as.formula(
    paste(
      "cbind(",
      paste(cand_cols, collapse = ", "),
      ") ~ cbind(",
      paste(race_cols, collapse = ", "),
      ")",
      sep = ""
    )
  )

  set.seed(seed)
  if (!is.null(totals_col)) { # When variables are percents #

    # Tune it real good #
    if (verbose == TRUE) {
      cat("\nTune the tuneMD real good...\n")
    }
    suppressWarnings(
      tune.nocov <- tuneMD(
        formula,
        data = data,
        ntunes = ntunes,
        totaldraws = totaldraws,
        total = totals_col,
        ...
      )
    )

    # Estimate Bayes Model -- can take a while (real good)
    if (verbose == TRUE) {
      cat("\nHello my name is Simon and I like to do ei.MD.bayes drawrings...\n")
    }
    suppressWarnings(
      md_out <- ei.MD.bayes(
        formula,
        data = data,
        sample = sample,
        total = totals_col,
        thin = thin,
        burnin = burnin,
        ret.mcmc = ret_mcmc,
        tune.list = tune.nocov,
        ...
      )
    )
  } else { # When variables are raw numeros #

    # Tune it so good #
    if (verbose == TRUE) {
      cat("\nTune the tuneMD real good...\n")
    }
    tune.nocov <- tuneMD(
      formula,
      data = data,
      ntunes = ntunes,
      totaldraws = totaldraws,
      ...
    )

    # Estimate Bayes Model real good
    if (verbose == TRUE) {
      cat("\nAnd you know my name is Simon and I like to do ei.MD.bayes() drawrings...\n")
    }
    md_out <- ei.MD.bayes(
      formula,
      data = data,
      sample = sample,
      thin = thin,
      burnin = burnin,
      ret.mcmc = ret_mcmc,
      tune.list = tune.nocov,
      ...
    )
  }

  # extract MD Bayes cells
  md_draw <- md_out$draws$Cell.counts

  race_list <- list()

  # loop along race
  for (i in seq_len(length(race_cols))) {
    # pull MD draws
    race_comb <- md_draw[, grep(race_cols[i], colnames(md_draw))]

    total <- apply(race_comb, 1, sum)
    v_fill <- matrix(NA, nrow = nrow(race_comb), ncol = ncol(race_comb))

    for (j in seq_len(ncol(v_fill))) {
      v_fill[, j] <- race_comb[, j] / total
    }

    # get confidence intervals
    if (ci_true) {
      qtile <- cbind(
        mcmcse::mcse.mat(v_fill) * 100,
        mcmcse::mcse.q.mat(v_fill, q = ci[1])[, 1] * 100,
        mcmcse::mcse.q.mat(v_fill, q = ci[2])[, 1] * 100
      )

      colnames(qtile) <- c(
        "Mean",
        "SE",
        paste(ci[1] * 100, collapse = ""),
        paste(ci[2] * 100, collapse = "")
      )
      rownames(qtile) <- candidates
    } else {
      # don't get confidence intervals
      qtile <- cbind(mcmcse::mcse.mat(v_fill) * 100)

      colnames(qtile) <- c("Mean", "SE")
      rownames(qtile) <- candidates
    }

    race_list[[i]] <- qtile
  }

  names(race_list) <- race

  # produce both the table and the draws for draw analysis
  # two-item list
  if (produce_draws) {
    return(list(table = race_list, draws = md_draw))
  } else {
    return(race_list)
  }
}
