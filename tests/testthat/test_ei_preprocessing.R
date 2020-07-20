context("Testing performance of ei_preprocessing functions")

test_that("standardize_votes() returns correct results", {
  votes <- empty_ei_df(2, 0, 2)
  votes$c1 <- c(1, 1)
  votes$c2 <- c(1, 1)
  totals <- c(2, 2)
  expected <- data.frame(
    "c1_p" = c(0.5, 0.5), 
    "c2_p" = c(0.5, 0.5)
  )

  expect_equal(standardize_votes(votes), expected)
})

test_that("check_diffs() gets conditions right", {
  
  vote_sums <- rep(1, 5)
  provided_totals <- rep(1, 5)
  max_dev <- 0.1
  avg_dev <- 0.025

  # exact match should return 2
  res <- check_diffs(
    vote_sums,
    provided_totals,
    max_dev,
    avg_dev
  )
  expect_equal(res$closeness, 2)
  expect_equal(res$deviates, rep(FALSE, 5))
  
  # near match should return 1
  vote_sums[1] <- 1.05
  res <- check_diffs(
    vote_sums,
    provided_totals,
    max_dev,
    avg_dev
  )
  expect_equal(res$closeness, 1)
  expect_equal(res$deviates, c(TRUE, rep(FALSE, 4)))

  # max diff > 0.1 should return 0
  vote_sums[1] <- 1.11
  res <- check_diffs(
    vote_sums,
    provided_totals,
    max_dev,
    avg_dev
  )
  expect_equal(res$closeness, 0)
  expect_equal(res$deviates, c(TRUE, rep(FALSE, 4)))
  
  # avg diff > 0.025 should return 0
  vote_sums <- rep(1.03, 5)
  res <- check_diffs(
    vote_sums,
    provided_totals,
    max_dev,
    avg_dev
  )
  expect_equal(res$closeness, 0)
  expect_equal(res$deviates, rep(TRUE, 5))
  
  # should return error if max_dev < 0
  max_dev <- -0.3
  expect_error(check_diffs(
    vote_sums, provided_totals, max_dev, avg_dev
  ))
  
  # should return error if avg_dev < 0
  max_dev <- 0.1
  avg_dev <- -0.3
  expect_error(check_diffs(
    vote_sums, provided_totals, max_dev, avg_dev
  ))
  
  # should return 0 for any deviation if max_dev = 0
  # and avg_dev = 0
  max_dev <- 0
  avg_dev <- 0
  res <- check_diffs(
    vote_sums,
    provided_totals,
    max_dev,
    avg_dev
  )
  expect_equal(res$closeness, 0)
})

test_that("clean_votes() handles all cases", {
  
  df <- empty_ei_df()
  df$r1 <- 1
  df$r2 <- 1
  df$t <- 2
  
  # base case works correctly
  res <- clean_votes(
    data = df, 
    cols = c('r1', 'r2'), 
    totals_col = 't', 
    verbose = FALSE,
    diagnostic = FALSE
  )
  expected <- data.frame(
    'r1_p' = c(0.5, 0.5),
    'r2_p' = c(0.5, 0.5)
  )
  expect_equal(res, expected)
  
  # message prints when things work well
  expect_message(
    clean_votes(
      data = df, 
      cols = c('r1', 'r2'), 
      totals_col = 't', 
      verbose = TRUE,
      diagnostic = FALSE
    )
  )
  
  # diagnostic works correctly
  res <- clean_votes(
    data = df, 
    cols = c('r1', 'r2'), 
    totals_col = 't', 
    verbose = F,
    diagnostic = T
  )
  expected <- data.frame(
    'r1_p' = c(0.5, 0.5),
    'r2_p' = c(0.5, 0.5),
    'deviates' = c(FALSE, FALSE)
  )
  expect_equal(res, expected)
  
  # message on minor deviation only where verbose = TRUE
  df$r1[1] <- 0.99
  df$r1[2] <- 1.01
  df$r2[1] <- 1.01
  expect_message(
    clean_votes(
      data = df, 
      cols = c('r1', 'r2'), 
      totals_col = 't', 
      verbose = TRUE,
      diagnostic = FALSE
    )
  )
  
  # upon violation, print warning
  df$r2 <- 1
  df$r1 <- c(10,1)
  expect_warning(
    clean_votes(
      data = df, 
      cols = c('r1', 'r2'), 
      totals_col = 't', 
      verbose = TRUE,
      diagnostic = FALSE
    )
  )
  
  # upon violation, return diagnostic column regardless of verbose
  res <- suppressWarnings({
    clean_votes(
      data = df, 
      cols = c('r1', 'r2'), 
      totals_col = 't', 
      verbose = FALSE,
      diagnostic = FALSE
    )
  })
  expected <- data.frame('deviates' = c(TRUE, FALSE))
  expect_equal(res, expected)
})