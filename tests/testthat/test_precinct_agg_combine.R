context("Testing precinct aggregation function.")

test_that("Precinct aggregate function correctly runs on BISG outputs.", {
  voter_file <- data.frame(
    precinct = c(1, 1, 2, 2),
    pred.whi = c(0.10, 0.20, 0.30, 0.40),
    pred.bla = c(0.40, 0.30, 0.20, 0.10),
    pred.his = c(0.10, 0.20, 0.30, 0.40),
    pred.asi = c(0.30, 0.20, 0.10, 0.00),
    pred.oth = c(0.10, 0.10, 0.10, 0.10)
  )
  # Expected aggregated proportions
  expected_agg <- data.frame(
    precinct = c(1, 2),
    pred.whi_prop = c(0.15, 0.35),
    pred.bla_prop = c(0.35, 0.15),
    pred.his_prop = c(0.15, 0.35),
    pred.asi_prop = c(0.25, 0.05),
    pred.oth_prop = c(0.10, 0.10)
  )
  # Run precinct agg combine function
  observed_agg <- precinct_agg_combine(
    voter_file = voter_file,
    group_col = "precinct",
    include_total = FALSE
  )
  # Check whether columns are equal to each other
  testthat::expect_true(all.equal(as.data.frame(observed_agg), expected_agg))

  # Rename columns
  colnames(voter_file) <- c("county", "whi", "bla", "his", "asi", "oth")
  colnames(expected_agg) <- c(
    "county",
    "whi_prop",
    "bla_prop",
    "his_prop",
    "asi_prop",
    "oth_prop"
  )
  # Run precinct agg combine function
  observed_agg <- precinct_agg_combine(
    voter_file = voter_file,
    group_col = "county",
    race_cols = c("whi", "bla", "his", "asi", "oth"),
    include_total = FALSE
  )

  # Check whether columns are equal to each other
  testthat::expect_true(all.equal(as.data.frame(observed_agg), expected_agg))
})

test_that("Precinct aggregate function correctly runs on ground truth race.", {
  voter_file <- data.frame(
    precinct = c(1, 1, 1, 1, 2, 2, 2, 2),
    race = c("BL", "WH", "NA", "MR", "BL", "WH", "BL", "BL")
  )
  expected_agg <- data.frame(
    precinct = c(1, 2),
    whi_prop = c(0.25, 0.25),
    bla_prop = c(0.25, 0.75),
    oth_prop = c(0.50, 0.00),
    whi_total = c(1, 1),
    bla_total = c(1, 3),
    oth_total = c(2, 0)
  )
  # Run precinct agg combine function
  observed_agg <- precinct_agg_combine(
    voter_file = voter_file,
    group_col = "precinct",
    true_race_col = "race",
    true_race_keys = list("whi" = "WH", "bla" = "BL", "oth" = c("NA", "MR")),
    include_total = TRUE
  )
  # Check whether columns are equal to each other
  testthat::expect_true(all.equal(as.data.frame(observed_agg), expected_agg))

  # Check for error
  testthat::expect_error(
    observed_agg <- precinct_agg_combine(
      voter_file = voter_file,
      group_col = "precinct",
      true_race_keys = list("whi" = "WH", "bla" = "BL", "oth" = c("NA", "MR")),
      include_total = TRUE
    )
  )
})

test_that("Precinct aggregate function correctly runs on both inputs.", {
  voter_file <- data.frame(
    precinct = c(1, 1, 2, 2),
    p.whi = c(0.10, 0.20, 0.30, 0.40),
    p.bla = c(0.40, 0.30, 0.20, 0.10),
    p.his = c(0.10, 0.20, 0.30, 0.40),
    p.asi = c(0.30, 0.20, 0.10, 0.00),
    p.oth = c(0.10, 0.10, 0.10, 0.10),
    race = c("BL", "WH", "BL", "WH")
  )
  # Test that both ground truth and estimated can be run at the same time
  expected_agg <- data.frame(
    precinct = c(1, 2),
    p.whi_prop = c(0.15, 0.35),
    p.bla_prop = c(0.35, 0.15),
    p.his_prop = c(0.15, 0.35),
    p.asi_prop = c(0.25, 0.05),
    p.oth_prop = c(0.10, 0.10),
    whi_prop = c(0.50, 0.50),
    bla_prop = c(0.50, 0.50)
  )
  # Run precinct agg combine function
  observed_agg <- precinct_agg_combine(
    voter_file = voter_file,
    group_col = "precinct",
    race_cols = c("p.whi", "p.bla", "p.his", "p.asi", "p.oth"),
    true_race_col = "race",
    true_race_keys = list("whi" = "WH", "bla" = "BL"),
    include_total = FALSE
  )
  # Check whether columns are equal to each other
  testthat::expect_true(all.equal(as.data.frame(observed_agg), expected_agg))

  # Test null input for race columns
  expected_agg <- data.frame(
    precinct = c(1, 2),
    whi_prop = c(0.50, 0.50),
    bla_prop = c(0.50, 0.50)
  )
  # Run precinct agg combine function
  observed_agg <- precinct_agg_combine(
    voter_file = voter_file,
    group_col = "precinct",
    race_cols = NULL,
    true_race_col = "race",
    true_race_keys = list("whi" = "WH", "bla" = "BL"),
    include_total = FALSE
  )
  # Check whether columns are equal to each other
  testthat::expect_true(all.equal(as.data.frame(observed_agg), expected_agg))
})
