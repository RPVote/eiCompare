context("Test Election Algebra function: elect_algebra")

test_that("elect_algebra outputs correctly", {
    
    totals <- data.frame(turnout = c(0.2786, 0.1663), cvap = c(36472, 23851))
    c1_ei_res <- c(0.2796, 0.7204)
    c2_ei_res <- c(0.7013, 0.2987)
    cand_names <- c("Cand A", "Cand B")
    
    output <- elect_algebra(totals = totals, c1_ei_res, c2_ei_res, cand_names)
    
    expected <- data.frame(
        "White_Vote" = c(2841, 7320, 10161),
        "NonWhite_Vote" = c(2781, 1185, 3966),
        "Total" = c(5622, 8505, 14127),
        "Pct_White" = c(0.28, 0.72, ""),
        "Pct_NonWhite" = c(0.701, 0.299, "")
    )
    
    rownames(expected) <- c("Cand A", "Cand B", "Total")
    
    expect_equal(output, expected)
    
})