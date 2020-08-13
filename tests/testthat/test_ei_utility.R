context("test EI utility functions")

test_that("ei results table mungs results correctly", {
  input <- list(
    data.frame(
      "Candidate" = c("x", "se"),
      "pct_race" = c(0.75, 0.25),
      "pct_other" = c(0.25, 0.2)
    ),
    data.frame(
      "Candidate" = c("y", "se"),
      "pct_race" = c(0.70, 0.20),
      "pct_other" = c(.30, 0.1)
    )
  )
  expected <- data.frame(
    "Candidate" = c("x", "se", "y", "se", "Total"),
    "pct_race" = c(0.75, 0.25, 0.70, 0.20, 1.45) * 100,
    "other" = c(0.25, 0.2, 0.30, 0.1, 0.55) * 100
  )
  output <- get_results_table(input,
    cand_col = c("x", "y"),
    race_col = c("pct_race"),
    n_race = 1,
    n_cand = 2,
    n_iter = 2
  )
  expect_equal(output, expected)
})

test_that("col_checker returns correct warnings", {
  input <- data.frame(
    "x" = NA,
    "y" = NA,
    "t" = NA
  )
  expect_silent(
    check_args(
      data = input,
      cand_cols = c("x"),
      race_cols = c("y"),
      totals_col = "t"
    )
  )
  expect_error(
    check_args(
      data = input,
      cand_cols = c("x"),
      race_cols = c("y"),
      totals_col = "g"
    )
  )
})
