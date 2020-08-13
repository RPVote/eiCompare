context("Testing performance of ei_propreprocessing functions")

test_that("dedupe_precincts handles cases correctly", {

  # unduplicated dataset returns the same result
  input <- data.frame("p" = c(1, 2), "e" = c(1, 1))
  expected <- input
  output <- suppressMessages(dedupe_precincts(input, "p"))
  expect_equal(output, expected)

  # message from verbose, no duplicates
  expect_message(dedupe_precincts(input, "p"))

  # duplicated full rows returns single row
  input$p[2] <- 1
  expected <- input[1, ]
  output <- suppressMessages(dedupe_precincts(input, "p"))
  expect_equal(output, expected)

  # duplicated precinct, different rows returns boolean column
  input[1, 2] <- 2
  expected <- cbind(input, data.frame("duplicate" = c(TRUE, TRUE)))
  output <- suppressWarnings(dedupe_precincts(input, "p"))
  expect_equal(output, expected)
})

test_that("resolve_missing_vals() handles cases correctly", {

  # Base case, no NAs
  input <- data.frame(
    "x" = rep(1, 3),
    "y" = rep(1, 3),
    "t" = rep(1, 3)
  )
  cand_cols <- c("x")
  race_cols <- c("y")
  totals_col <- "t"

  expected <- input
  output <- resolve_missing_vals(
    data = input,
    cand_cols = cand_cols,
    race_cols = race_cols,
    totals_col = totals_col,
    verbose = FALSE
  )
  expect_equal(output, expected)

  # NA in column imputes mean when na_action == "mean"
  input$y[1] <- NA

  output <- resolve_missing_vals(
    data = input,
    cand_cols = cand_cols,
    race_cols = race_cols,
    totals_col = totals_col,
    na_action = "mean",
    verbose = FALSE
  )
  expect_equal(output, expected)

  # NA in column removes column when na_action == "drop"
  input$x[1] <- NA
  expected <- expected[-1, ]
  output <- resolve_missing_vals(
    data = input,
    cand_cols = cand_cols,
    race_cols = race_cols,
    totals_col = totals_col,
    verbose = FALSE
  )
  expect_equal(output, expected)
})


test_that("standardize_votes() returns correct results", {
  votes <- empty_ei_df(2, 0, 2)
  votes$c1 <- c(1, 1)
  votes$c2 <- c(1, 1)
  totals <- c(2, 2)
  expected <- data.frame(
    "c1_prop" = c(0.5, 0.5),
    "c2_prop" = c(0.5, 0.5),
    "total" = c(2, 2)
  )

  expect_equal(standardize_votes(votes, new_names = TRUE), expected)
})

test_that("check_diffs() gets conditions right", {
  vote_sums <- rep(1, 5)
  provided_totals <- rep(1, 5)
  max_dev <- 0.1
  avg_dev <- 0.025

  # Exact match should return 2
  res <- check_diffs(
    vote_sums,
    provided_totals,
    max_dev,
    avg_dev
  )
  expect_equal(res$closeness, 2)
  expect_equal(res$deviates, rep(FALSE, 5))

  # Near match should return 1
  vote_sums[1] <- 1.05
  res <- check_diffs(
    vote_sums,
    provided_totals,
    max_dev,
    avg_dev
  )
  expect_equal(res$closeness, 1)
  expect_equal(res$deviates, c(TRUE, rep(FALSE, 4)))

  # Max diff > 0.1 should return 0
  vote_sums[1] <- 1.11
  res <- check_diffs(
    vote_sums,
    provided_totals,
    max_dev,
    avg_dev
  )
  expect_equal(res$closeness, 0)
  expect_equal(res$deviates, c(TRUE, rep(FALSE, 4)))

  # Avg diff > 0.025 should return 0
  vote_sums <- rep(1.03, 5)
  res <- check_diffs(
    vote_sums,
    provided_totals,
    max_dev,
    avg_dev
  )
  expect_equal(res$closeness, 0)
  expect_equal(res$deviates, rep(TRUE, 5))

  # Should return error if max_dev < 0
  max_dev <- -0.3
  expect_error(check_diffs(
    vote_sums, provided_totals, max_dev, avg_dev
  ))

  # Should return error if avg_dev < 0
  max_dev <- 0.1
  avg_dev <- -0.3
  expect_error(check_diffs(
    vote_sums, provided_totals, max_dev, avg_dev
  ))

  # Should return 0 for any deviation if max_dev = 0 and avg_dev = 0
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

test_that("stdize_votes() handles all cases", {
  df <- empty_ei_df()
  df$r1 <- 1
  df$r2 <- 1
  df$t <- 2

  # Base case works correctly
  res <- stdize_votes(
    data = df,
    cols = c("r1", "r2"),
    totals_col = "t",
    new_names = TRUE,
    verbose = FALSE,
    diagnostic = FALSE
  )
  expected <- data.frame(
    "r1_prop" = c(0.5, 0.5),
    "r2_prop" = c(0.5, 0.5),
    "total" = c(2, 2)
  )
  expect_equal(res, expected)

  # Message prints when things work well
  expect_message(
    stdize_votes(
      data = df,
      cols = c("r1", "r2"),
      totals_col = "t",
      new_names = TRUE,
      verbose = TRUE,
      diagnostic = FALSE
    )
  )

  # Diagnostic works correctly
  res <- stdize_votes(
    data = df,
    cols = c("r1", "r2"),
    totals_col = "t",
    new_names = TRUE,
    verbose = F,
    diagnostic = T
  )
  expected <- data.frame(
    "r1_prop" = c(0.5, 0.5),
    "r2_prop" = c(0.5, 0.5),
    "total" = c(2, 2),
    "deviates" = c(FALSE, FALSE)
  )
  expect_equal(res, expected)

  # Message on minor deviation only where verbose = TRUE
  df$r1[1] <- 0.99
  df$r1[2] <- 1.01
  df$r2[1] <- 1.01
  expect_message(
    stdize_votes(
      data = df,
      cols = c("r1", "r2"),
      totals_col = "t",
      new_names = TRUE,
      verbose = TRUE,
      diagnostic = FALSE
    )
  )

  # Correct values on minor deviation
  res <- stdize_votes(
    data = df,
    cols = c("r1", "r2"),
    totals_col = "t",
    new_names = TRUE,
    verbose = TRUE,
    diagnostic = FALSE
  )

  r11 <- 0.99 / (0.99 + 1.01)
  r12 <- 1.01 / (1 + 1.01)
  r21 <- 1.01 / (0.99 + 1.01)
  r22 <- 1 / (1 + 1.01)

  expected <- data.frame(
    "r1_prop" = c(r11, r12),
    "r2_prop" = c(r21, r22),
    "total" = c(2.00, 2.01)
  )
  expect_equal(expected, res)

  # Upon violation, print warning
  df$r2 <- 1
  df$r1 <- c(10, 1)
  expect_warning(
    stdize_votes(
      data = df,
      cols = c("r1", "r2"),
      totals_col = "t",
      new_names = TRUE,
      verbose = TRUE,
      diagnostic = FALSE
    )
  )

  # Upon violation, return diagnostic column regardless of verbose
  res <- suppressWarnings({
    stdize_votes(
      data = df,
      cols = c("r1", "r2"),
      totals_col = "t",
      new_names = TRUE,
      verbose = FALSE,
      diagnostic = FALSE
    )
  })
  expected <- data.frame("deviates" = c(TRUE, FALSE))
  expect_equal(res, expected)
})

test_that("stdize_votes_all() handles all cases", {
  df <- empty_ei_df()
  df[1, ] <- 1
  df[2, ] <- 1

  # Base case works correctly
  res <- stdize_votes_all(
    data = df,
    race_cols = c("r1", "r2"),
    cand_cols = c("c1", "c2"),
    new_names = TRUE
  )
  expected <- data.frame(
    "c1_prop" = rep(0.5, 2),
    "c2_prop" = rep(0.5, 2),
    "r1_prop" = rep(0.5, 2),
    "r2_prop" = rep(0.5, 2),
    "total" = c(2, 2)
  )
  expect_equal(res, expected)

  # Deviation in cand breaks race when totals_from = 'cand'
  df$c1[1] <- 9
  res <- suppressWarnings({
    stdize_votes_all(
      data = df,
      race_cols = c("r1", "r2"),
      cand_cols = c("c1", "c2"),
      new_names = TRUE
    )
  })
  expected <- data.frame(
    "c1_prop" = c(0.9, 0.5),
    "c2_prop" = c(0.1, 0.5),
    "total" = c(10, 2),
    "race_deviates" = c(TRUE, FALSE)
  )
  expect_equal(res, expected)

  # Deviation in cand breaks cand when totals_from = 'race'
  res <- suppressWarnings({
    stdize_votes_all(
      data = df,
      race_cols = c("r1", "r2"),
      cand_cols = c("c1", "c2"),
      totals_from = "race",
      new_names = TRUE
    )
  })
  expected <- data.frame(
    "r1_prop" = c(0.5, 0.5),
    "r2_prop" = c(0.5, 0.5),
    "total" = c(2, 2),
    "cand_deviates" = c(TRUE, FALSE)
  )
  expect_equal(res, expected)

  # Deviation in race breaks cand when totals_from = 'race'
  df$c1[1] <- 1
  df$r1[1] <- 9
  res <- suppressWarnings({
    stdize_votes_all(
      data = df,
      race_cols = c("r1", "r2"),
      cand_cols = c("c1", "c2"),
      totals_from = "race",
      new_names = TRUE
    )
  })
  expected <- data.frame(
    "r1_prop" = c(0.9, 0.5),
    "r2_prop" = c(0.1, 0.5),
    "total" = c(10, 2),
    "cand_deviates" = c(TRUE, FALSE)
  )
  expect_equal(res, expected)

  # Deviation in race breaks race when totals_from = 'cand'
  res <- suppressWarnings({
    stdize_votes_all(
      data = df,
      race_cols = c("r1", "r2"),
      cand_cols = c("c1", "c2"),
      totals_from = "cand",
      new_names = TRUE
    )
  })
  expected <- data.frame(
    "c1_prop" = c(0.5, 0.5),
    "c2_prop" = c(0.5, 0.5),
    "total" = c(2, 2),
    "race_deviates" = c(TRUE, FALSE)
  )
  expect_equal(res, expected)

  # Totals_col works in base case
  df$r1 <- 1
  df$t <- 2
  res <- suppressMessages({
    stdize_votes_all(
      data = df,
      race_cols = c("r1", "r2"),
      cand_cols = c("c1", "c2"),
      totals_col = "t",
      new_names = TRUE
    )
  })
  expected <- data.frame(
    "c1_prop" = rep(0.5, 2),
    "c2_prop" = rep(0.5, 2),
    "r1_prop" = rep(0.5, 2),
    "r2_prop" = rep(0.5, 2),
    "total" = c(2, 2)
  )
  expect_equal(res, expected)

  # Both break when a total deviates
  df$t[1] <- 10
  res <- suppressWarnings({
    stdize_votes_all(
      data = df,
      race_cols = c("r1", "r2"),
      cand_cols = c("c1", "c2"),
      totals_col = "t",
      new_names = TRUE
    )
  })
  expected <- data.frame(
    "cand_deviates" = c(TRUE, FALSE),
    "race_deviates" = c(TRUE, FALSE)
  )
})
