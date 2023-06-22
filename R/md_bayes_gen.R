#' MD Bayes Generalize
#'
#' Tunes and estimates MD Bayes algorithm (ei.MD.bayes). This, combined with
#' md_bayes_table() produces tables of results compatible with EI table of
#' results.
#'
#'
#' @param dat data.frame() object of just raw candidate vote and raw population
#' counts. Put vote results in first set of columns, put population counts next
#' @param form Formula object, e.g.: cbind(V1, V2, novote) ~ cbind(VtdAVap_cor,
#' VtdBVap_cor, VtdHVap_cor, VtdOVap_cor)
#' @param total_yes Logical, default=TRUE. Include total variable from data?
#' Usually when data are stored in percents
#' @param total character, total variable column name
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
#' \donttest{
#' # TOY DATA EXAMPLE
#' canda <- c(10, 8, 10, 4, 8)
#' candb <- 20 - canda
#' white <- c(15, 12, 18, 6, 10)
#' black <- 20 - white
#' toy <- data.frame(canda, candb, white, black)
#'
#' # Generate formula for passage to ei.reg.bayes() function #
#' form <- formula(cbind(canda, candb) ~ cbind(black, white))
#'
#' # Then execute md_bayes_gen(); not run here due to time
#' md_bayes_gen(
#'   dat = toy,
#'   form = form,
#'   total_yes = FALSE,
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
#' }
#' @importFrom stringr str_squish
#' @importFrom mcmcse mcse.mat mcse.q.mat
#' @export md_bayes_gen
md_bayes_gen <- function(dat, form, total_yes = TRUE, total, ntunes = 10,
                         totaldraws = 10000, seed = 12345, sample = 1000,
                         thin = 100, burnin = 10000, ret_mcmc = TRUE,
                         ci = c(0.025, 0.975), ci_true = TRUE,
                         produce_draws = FALSE, ...) {
  set.seed(seed)
  # when variables are percents
  if (total_yes) {
    suppressWarnings(tune_nocov <- tuneMD(form,
      data = dat, ntunes = ntunes,
      totaldraws = totaldraws, total = total, ...
    ))

    # estimate bayes model - can take a while
    suppressWarnings(md_out <- ei.MD.bayes(form,
      data = dat, sample = sample, total = total,
      thin = thin, burnin = burnin, ret.mcmc = ret_mcmc,
      tune.list = tune_nocov, ...
    ))
    # when variables are raw numbers
  } else {
    # tune the MD Bayes model
    tune_nocov <- tuneMD(form,
      data = dat, ntunes = ntunes,
      totaldraws = totaldraws, ...
    )

    # estimate Bayes model
    md_out <- ei.MD.bayes(form,
      data = dat, sample = sample,
      thin = thin, burnin = burnin, ret.mcmc = ret_mcmc,
      tune.list = tune_nocov, ...
    )
  }

  # extract MD Bayes cells
  md_draw <- md_out$draws$Cell.counts

  # clean up formula
  name_extract_rxc <- function(form_object, num) {
    form <- gsub("cbind(", "", as.character(form)[num], fixed = T)
    form <- gsub(")", "", form, fixed = T)
    var <- unlist(strsplit(form, ","))
    stringr::str_squish(var)
  }

  # race and candidates
  race <- name_extract_rxc(form, 3)
  candidates <- name_extract_rxc(form, 2)

  race_list <- list()

  # loop along race
  for (i in seq_len(length(race))) {
    # pull MD draws
    race_comb <- md_draw[, grep(race[i], colnames(md_draw))]

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
