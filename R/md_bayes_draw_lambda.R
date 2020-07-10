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
  tune.nocov <- tuneMD(formula1,
    data = dat, ntunes = ntunes,
    totaldraws = totaldraws
  )
  md.out <- ei.MD.bayes(formula1,
    data = dat, sample = sample,
    thin = thin, burnin = burnin, ret.mcmc = ret.mcmc, tune.list = tune.nocov
  )
  cat(paste("Taking first ", race_vote_split[1], " names from dat object\n", sep = ""))
  lmd <- lambda.MD(md.out, columns = names(dat[, 1:race_vote_split[1]]))
  return(lmd)
}
