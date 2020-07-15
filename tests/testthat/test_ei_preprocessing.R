context("Testing performance of ei_preprocessing functions")

test_that("standardize_votes() returns correct results", {
  votes <- empty_ei_df(2, 0, 2)
  votes$c1 <- c(1,1)
  votes$c2 <- c(1,1)
  totals <- c(2,2)
  expected <- data.frame('c1_prp' = c(0.5, 0.5), 'c2_prp' = c(0.5, 0.5))
  
  expect_equal(standardize_votes(votes), expected)
})

test_that("check_race_diffs gets conditions right", {
  vote_sums <- rep(1, 5)
  provided_totals <- rep(1, 5)
  max_dev <- 0.1
  avg_dev <- 0.025

  expect_equal(
    check_race_diffs(
      vote_sums,
      provided_totals,
      max_dev,
      avg_dev
    ),
    2
  )
  vote_sums[1] <- 1.05
  expect_equal(
    check_race_diffs(
      vote_sums,
      provided_totals,
      max_dev,
      avg_dev
    ),
    1
  )
  vote_sums[1] <- 1.11
  expect_equal(
    check_race_diffs(
      vote_sums,
      provided_totals,
      max_dev,
      avg_dev
    ),
    0
  )
})