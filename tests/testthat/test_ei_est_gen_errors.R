context("ei_est_gen error handling")

testthat::test_that("ei_est_gen returns error when called without parameters", {
  testthat::expect_error(ei_est_gen())
})