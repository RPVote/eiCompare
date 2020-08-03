context("Testing WRU predict race wrapper function.")

test_that("WRU wrapper correctly calculates probabilities.", {
  library(wru)

  # Create voter file
  voter_file <- data.frame(
    voter_id = c("1", "2"),
    surname = c("JOHNSON", "HERNANDEZ"),
    state = "NY",
    county = c("087", "087"),
    tract = c("010101", "010101"),
    block = c("1001", "1016")
  )

  # Load Rockland county Census information
  data(rockland_census)

  # Run predict race wrapper function
  bisg <- wru_predict_race_wrapper(
    voter_file = voter_file,
    census_data = rockland_census,
    voter_id = "voter_id",
    surname = "surname",
    state = "NY",
    county = "county",
    tract = "tract",
    block = "block",
    census_geo = "block",
    use_surname = TRUE,
    surname_only = FALSE,
    surname_year = 2010,
    use_age = FALSE,
    use_sex = FALSE,
    return_surname_flag = TRUE,
    return_geocode_flag = TRUE,
    verbose = FALSE
  )
  expect_true(all(!is.na(bisg$bisg)))
  expect_true(all(bisg$voter_file$merged_surname))
  expect_true(all(bisg$voter_file$merged_geocode))


  # Run predict race wrapper function
  bisg <- wru_predict_race_wrapper(
    voter_file = voter_file,
    census_data = rockland_census,
    voter_id = "voter_id",
    surname = "surname",
    state = "NY",
    county = "county",
    tract = "tract",
    block = "block",
    census_geo = "block",
    use_surname = TRUE,
    surname_only = TRUE,
    surname_year = 2010,
    use_age = FALSE,
    use_sex = FALSE,
    return_surname_flag = TRUE,
    return_geocode_flag = TRUE,
    verbose = FALSE
  )
  expect_true(all(!is.na(bisg$bisg)))
  testthat::expect_false(any(names(bisg$voter_file) == "merged_geocode"))
})
