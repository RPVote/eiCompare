#' MD Bayes Draw Lambda
#'
#' Tunes and estimates MD Bayes algorithm (ei.MD.bayes). Returns a data frame
#' of lambda posterior distribution draws. Similar to md_bayes_draw, but used
#' primarily for assessing posterior distribution tests.
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
#' @return Posterior distribution of lambdas. This is often used for assessing
#' RPB in elections with a small number of precincts.
#' @author Loren Collingwood <loren.collingwood@@ucr.edu>; Justin Gross
#' <jhgross@@umass.edu>
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
#' # Generate formula for passage to ei.reg.bayes() function
#' form <- formula(cbind(canda, candb) ~ cbind(black, white))
#' # Then excute md_bayes_draw(); not run here due to time
#' md_bayes_draw_lambda(toy, c(2,3), form )
#' }
#' @import eiPack
#' @export md_bayes_draw_lambda
md_bayes_draw_lambda <- function(dat, race_vote_split, form, ntunes = 10, totaldraws = 1e+05,
                                 seed = 12345, sample = 1e+05, thin = 100, burnin = 1e+05,
                                 ret.mcmc = TRUE) {
  left_dat <- apply(dat[, 1:race_vote_split[1]], 1, sum)
  right_dat <- apply(
    dat[, race_vote_split[2]:ncol(dat)], 1,
    sum
  )
  stopifnot(all.equal(left_dat, right_dat))
  formula1 <- form
  set.seed(seed)
  tune.nocov <- eiPack::tuneMD(formula1,
    data = dat, ntunes = ntunes,
    totaldraws = totaldraws
  )
  md.out <- eiPack::ei.MD.bayes(formula1,
    data = dat, sample = sample,
    thin = thin, burnin = burnin, ret.mcmc = ret.mcmc, tune.list = tune.nocov
  )
  message(paste("Taking first ", race_vote_split[1], " names from dat object\n", sep = ""))
  lmd <- eiPack::lambda.MD(md.out, columns = names(dat[, 1:race_vote_split[1]]))
  return(lmd)
}
