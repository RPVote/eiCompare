lambda_two_compare <- function(lmd, cnames, group_name = "Latino",
                               cand1or2 = 1) {
  if (cand1or2 == 2) {
    cnames <- rev(cnames)
    p.cand_over_cand <- lmd[, cnames[2]] - lmd[, cnames[1]]
  }

  n <- nrow(lmd)
  labels <- sub(".*\\.", "", cnames)

  # what is probability that Morales is preferred to Rothman among Latinos?
  p.cand_over_cand <- lmd[, cnames[1]] - lmd[, cnames[2]]

  # Plot Histogram & Posterior #
  par(mfrow = c(1, 2))

  # Histogram #
  hist(lmd[, cnames[1]] - lmd[, cnames[2]],
    main = paste(labels[1], " - ", labels[2], sep = ""),
    xlab = "Posterior Distribution"
  )

  # Density #

  # Need regular expression for everything to right of period to extract names #
  dens <- density(p.cand_over_cand)
  plot(dens,
    main = paste("Difference in Proportion ", group_name,
      "\nVote for ", labels[1], " and ", labels[2],
      sep = ""
    ),
    xlab = "Difference in Posterior Distribution Sampled from "
  )
  points(p.cand_over_cand, rep(0, length(p.cand_over_cand)), pch = 3) # plot individual posterior sampled points (no)
  abline(v = 0, col = "grey", lty = 2)

  # Probability 1 Candidate over Another #
  c1g10 <- length(which(p.cand_over_cand > .10)) / n # Morales over Rothman by at least 10 points
  c1g5 <- length(which(p.cand_over_cand > .05)) / n # # Morales over Rothman by at least 5 points
  c1g0 <- length(which(p.cand_over_cand > 0)) / n

  med <- median(p.cand_over_cand)
  mean <- mean(p.cand_over_cand)
  df <- data.frame(c1g10, c1g5, c1g0, round(med, 3), round(mean, 3))
  colnames(df) <- c("Prob>10%", "Prob>5%", "Prob>0", "Dist. Median", "Dist. Mean")
  return(df)
}
