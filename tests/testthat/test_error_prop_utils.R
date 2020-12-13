context("Test error propagation utility functions.")

test_that("compute_product_err calculates error correctly", {
    p_g_r <- data.frame(x = c(0.5), y = c(0.2), z = c(0.1))
    p_g_r_err <- data.frame(x = c(0.1), y = c(0.05), z = c(0.02))
    p_r_s <- data.frame(x = c(0.2), y = c(0.1), z = c(0.7))
    p_r_s_err <- data.frame(x = c(0.04), y = c(0.01), z = c(0.2))
    expected_err <- eiCompare::compute_product_err(
        p_g_r = p_g_r,
        p_r_s = p_r_s,
        p_g_r_err = p_g_r_err,
        p_r_s_err = p_r_s_err
    )
    p_g_r_samples <- matrix(
        rnorm(
            n = 300000,
            mean = as.numeric(p_g_r),
            sd = sqrt(as.numeric(p_g_r_err))
        ),
        byrow = TRUE,
        ncol = 3
    )
    p_r_s_samples <- matrix(
        rnorm(
            n = 300000,
            mean = as.numeric(p_r_s),
            sd = sqrt(as.numeric(p_r_s_err))
        ),
        byrow = TRUE,
        ncol = 3
    )
    observed_err <- matrixStats::colVars(p_g_r_samples * p_r_s_samples)
    testthat::expect_equal(observed_err, expected_err, tolerance = 0.1)
})