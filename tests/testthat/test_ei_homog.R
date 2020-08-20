context("Test homogeneous precinct analysis function: ei_homog")

test_that("ei_homog outputs correctly", {
    
    input <- data.frame(
            "cand_a" = c( rep(.8, 10), rep(.2, 10)),
            "cand_b" = 1 - c( rep(.8, 10), rep(.2, 10)),
            "white" = c(rep(.7, 5), rep(.85, 5), rep(.1, 5), rep(.05, 5)),
            "black" = 1 - c(rep(.7, 5), rep(.85, 5), rep(.1, 5), rep(.05, 5)),
            "total" = c ( rep(200, 5), rep(100, 5), rep(80, 5), rep(300, 5) )
    )

    expected <- data.frame(
        "white" = c(0.8, 0.2),
        "black" = c(0.2, 0.8)
    )
    rownames(expected) <- c("cand_a", "cand_b")
    expected <- as.matrix(expected)
    
    output <- ei_homog(data = input,
                       race_cols = c("white", "black"),
                       cand_cols = c("cand_a", "cand_b"),
                       totals_col = "total"
    )
    
    expect_equal(output, expected)
    
})