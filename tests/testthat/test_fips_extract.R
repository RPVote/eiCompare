context("Testing FIPS extractor function.")


test_that("FIPS extractor correctly splits codes of various lengths.", {
  fips_codes <- data.frame(
    fips = c(
      "112223333334444",
      "11222333333",
      "11222",
      "11"
    )
  )
  expected_state <- c("11", "11", "11", "11")
  expected_county <- c("222", "222", "222", "")
  expected_tract <- c("333333", "333333", "", "")
  expected_block <- c("4444", "", "", "")

  observed_split <- fips_extract(
    df = fips_codes,
    fips_col = "fips",
    geo = NULL
  )
  # Check whether columns are equal to each other
  testthat::expect_equal(observed_split$state, expected_state)
  testthat::expect_equal(observed_split$county, expected_county)
  testthat::expect_equal(observed_split$tract, expected_tract)
  testthat::expect_equal(observed_split$block, expected_block)
})


test_that("FIPS extractor works for int input.", {
  fips_codes <- data.frame(
    fips = c(
      1122233333344445555,
      11222333333,
      11222,
      11
    )
  )
  expected_state <- c("11", "11", "11", "11")
  expected_county <- c("222", "222", "222", "")
  expected_tract <- c("333333", "333333", "", "")
  expected_block <- c("4444", "", "", "")

  testthat::expect_error(
    observed_split <- fips_extract(
      df = fips_codes,
      fips_col = "fips",
      geo = geo
    )
  )
})


test_that("FIPS extractor correctly handles geographic unit argument.", {
  # Test when we split at the block level
  fips_codes <- data.frame(
    fips = c(
      "112223333334444",
      "11222333333",
      "11222",
      "11"
    )
  )
  geo <- "block"
  expected_state <- c("11", "11", "11", "11")
  expected_county <- c("222", "222", "222", "")
  expected_tract <- c("333333", "333333", "", "")
  expected_block <- c("4444", "", "", "")

  observed_split <- fips_extract(
    df = fips_codes,
    fips_col = "fips",
    geo = geo
  )
  # Check whether columns are equal to each other
  testthat::expect_equal(observed_split$state, expected_state)
  testthat::expect_equal(observed_split$county, expected_county)
  testthat::expect_equal(observed_split$tract, expected_tract)
  testthat::expect_equal(observed_split$block, expected_block)

  # Test when we split at the tract level
  geo <- "tract"
  expected_state <- c("11", "11", "11", "11")
  expected_county <- c("222", "222", "222", "")
  expected_tract <- c("3333334444", "333333", "", "")

  observed_split <- fips_extract(
    df = fips_codes,
    fips_col = "fips",
    geo = geo
  )
  # Check whether columns are equal to each other
  testthat::expect_equal(observed_split$state, expected_state)
  testthat::expect_equal(observed_split$county, expected_county)
  testthat::expect_equal(observed_split$tract, expected_tract)
  testthat::expect_false(any(names(observed_split) == "block"))

  # Test when we split at the county level
  geo <- "county"
  expected_state <- c("11", "11", "11", "11")
  expected_county <- c("2223333334444", "222333333", "222", "")

  observed_split <- fips_extract(
    df = fips_codes,
    fips_col = "fips",
    geo = geo
  )
  # Check whether columns are equal to each other
  testthat::expect_equal(observed_split$state, expected_state)
  testthat::expect_equal(observed_split$county, expected_county)
  testthat::expect_false(any(names(observed_split) == "block"))
  testthat::expect_false(any(names(observed_split) == "tract"))

  # Test that error is thrown when we split at the state level
  geo <- "state"
  testthat::expect_error(
    observed_split <- fips_extract(
      df = fips_codes,
      fips_col = "fips",
      geo = geo
    )
  )

  # Test that error is thrown when we enter nonsensical string for geo
  geo <- "foo"
  testthat::expect_error(
    observed_split <- fips_extract(
      df = fips_codes,
      fips_col = "fips",
      geo = geo
    )
  )
})


test_that("FIPS extractor uses first column when no column is provided.", {
  fips_codes <- data.frame(
    fips = c(
      "112223333334444",
      "11222333333",
      "11222",
      "11"
    )
  )
  expected_state <- c("11", "11", "11", "11")
  expected_county <- c("222", "222", "222", "")
  expected_tract <- c("333333", "333333", "", "")
  expected_block <- c("4444", "", "", "")

  observed_split <- fips_extract(
    df = fips_codes,
    fips_col = NULL,
    geo = NULL
  )
  # Check whether columns are equal to each other
  testthat::expect_equal(observed_split$state, expected_state)
  testthat::expect_equal(observed_split$county, expected_county)
  testthat::expect_equal(observed_split$tract, expected_tract)
  testthat::expect_equal(observed_split$block, expected_block)
})


test_that("FIPS extractor throws an error for incorrect FIPS length.", {
  fips_codes <- data.frame(
    fips = c(
      "1122233333344445555",
      "11222333333",
      "11222",
      "11"
    )
  )
  expected_state <- c("11", "11", "11", "11")
  expected_county <- c("222", "222", "222", "")
  expected_tract <- c("333333", "333333", "", "")
  expected_block <- c("4444", "", "", "")

  testthat::expect_error(
    observed_split <- fips_extract(
      df = fips_codes,
      fips_col = "fips",
      geo = geo
    )
  )
})
