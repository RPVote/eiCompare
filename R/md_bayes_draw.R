#' MD Bayes Draw
#'
#' Tunes and estimates MD Bayes algorithm (ei.MD.bayes)
#'
#'
#' @param dat data.frame() object of just raw candidate vote and raw population
#' counts. Put vote results in first set of columns, put population counts next
#' @param race_vote_split Numeric vector of length 2 indicating where vote
#' column ends (e.g., 3), and population counts begin (e.g., 4): c(3,4)
#' @param form Formula object, e.g.: cbind(V1, V2, novote) ~ cbind(VtdAVap_cor,
#' VtdBVap_cor, VtdHVap_cor, VtdOVap_cor)
#' @param ntunes Numeric; how much to tune tuneMD. Default = 10
#' @param totaldraws Numeric; How many total draws from MD. Default = 100000
#' @param seed Numeric. Default = 12345
#' @param sample Numeric. Default = 100000
#' @param thin Numeric. Default = 10
#' @param burnin Numeric. Default = 100000
#' @param ret.mcmc Logical. Default = TRUE
#' @return Matrix object, of simulation reults
#' @author Loren Collingwood <loren.collingwood@@ucr.edu>
#' @references eiPack, King et. al. (http://gking.harvard.edu/eiR)
#' @import eiPack
#' @export md_bayes_draw
md_bayes_draw <- function(dat, race_vote_split, form,
                          ntunes = 10, totaldraws = 100000, seed = 12345,
                          sample = 100000, thin = 100, burnin = 100000,
                          ret.mcmc = TRUE) {
  # Check to see if vote column totals line up with vote total columns #
  left_dat <- apply(dat[, 1:race_vote_split[1]], 1, sum)
  right_dat <- apply(dat[, race_vote_split[2]:ncol(dat)], 1, sum)
  stopifnot(all.equal(left_dat, right_dat))

  # set formula #
  formula1 <- form
  set.seed(seed)
  # Tune #
  tune.nocov <- eiPack::tuneMD(formula1, data = dat, ntunes = ntunes, totaldraws = totaldraws)

  # Estimate Model #
  md.out <- eiPack::ei.MD.bayes(formula1,
    data = dat, sample = sample, thin = thin,
    burnin = burnin, ret.mcmc = ret.mcmc, tune.list = tune.nocov
  )
  message(summary(md.out))

  # Extract the simulations
  md_draw <- md.out$draws$Cell.counts

  # Return the simulations in a matrix, basically, mcmc class though
  return(md_draw)
}
