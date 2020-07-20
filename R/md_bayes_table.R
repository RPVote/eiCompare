md_bayes_table <- function(md_results) {
  if (paste(names(md_results), collapse = "") == paste(c("table", "draws"), collapse = "")) {
    md_results <- md_results$table
  }
  # Create various iterator/indexing objects #
  len <- nrow(md_results[[1]])
  rnames <- row.names(md_results[[1]])
  num_groups <- length(md_results)
  group_names <- names(md_results)

  seq_split <- 2:len

  # For tables with just one candidate (somehow) #
  if (len == 1) {
    rn <- c(rnames, "se", "Total")
  } else { # More than one candidate #

    rn <- c(
      R.utils::insert(rnames,
        ats = seq_split,
        values = rep("se", len - 1)
      ),
      "se", "Total"
    )
  }

  # Result NA matrix Holder #
  result_mat <- matrix(NA, ncol = num_groups, nrow = (len * 2) + 1)

  for (j in 1:num_groups) { # open up j loop

    n <- len * 2
    fill <- rep(NA, n)

    odd <- seq(1, n, 2) # odd
    even <- seq(2, n, 2) # odd

    fill[odd] <- md_results[[j]][, 1]
    fill[even] <- md_results[[j]][, 2]

    result_mat[, j] <- c(fill, sum(fill[odd]))
  } # close j loop

  # Label Columns #
  colnames(result_mat) <- paste("RxC", group_names, sep = "_")

  # Combine into Dataframe for ei_rc_good_table() function #
  result_mat <- data.frame(Candidate = rn, result_mat, stringsAsFactors = F)

  return(result_mat)
}
