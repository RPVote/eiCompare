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
  tune.nocov <- tuneMD(formula1, data = dat, ntunes = ntunes, totaldraws = totaldraws)

  # Estimate Model #
  md.out <- ei.MD.bayes(formula1,
    data = dat, sample = sample, thin = thin,
    burnin = burnin, ret.mcmc = ret.mcmc, tune.list = tune.nocov
  )
  print(summary(md.out))

  # Extract the simulations
  md_draw <- md.out$draws$Cell.counts

  # Return the simulations in a matrix, basically, mcmc class though
  return(md_draw)
}
