context("Test BISG utility functions.")

test_that("swap_census_geography operates correctly", {
    expect_equal("block group", eiCompare::swap_census_geography("block"))
    expect_equal("tract", eiCompare::swap_census_geography("block group"))
    expect_equal("county", eiCompare::swap_census_geography("tract"))
    expect_equal("state", eiCompare::swap_census_geography("county"))
    expect_equal("block group", eiCompare::swap_census_geography("block"))
    expect_error(eiCompare::swap_census_geography("test"))
})