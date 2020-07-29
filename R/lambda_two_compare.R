#' Lambda Two Compare
#'
#' Compares two vectors of lambdas, usually one racial group's support for two
#' separate candidates, or two separate groups' support for the same candidate.
#'
#'
#' @param lmd data.frame() object returned from md_bayes_draw_lambda()
#' @param cnames Vector of character (column) names, needs to match relevant
#' column names in md_bayes_draw_lambda return.
#' @param group_name Character string for name appearing in posterio plot.
#' Default is "Latino")
#' @param cand1or2 Numeric. Either 1 or 2. Default = 1. Which pairing over the
#' other.
#' @return Data frame of the probability of one scenario over the other by 10
#' pct., by 5 pct., greater than 0 (e.g., what is the probability that
#' candidate 1 beats candidate 2 among Latinos by 10 percentage points, etc.)
#' @author Loren Collingwood <loren.collingwood@@ucr.edu>; Justin Gross
#' <jhgross@@umass.edu>
#' @references eiPack, King et. al. (http://gking.harvard.edu/eiR)
#' @examples
#' # TOY DATA EXAMPLE
#' \dontrun{
#' canda <- c(10, 8, 10, 4, 8)
#' candb <- 20 - canda
#' white <- c(15, 12, 18, 6, 10)
#' black <- 20 - white
#' toy <- data.frame(canda, candb, white, black)
#'
#' # Generate formula for passage to ei.reg.bayes() function
#' form <- formula(cbind(canda, candb) ~ cbind(black, white))
#' # Then excute md_bayes_draw(); not run here due to time
#' # lmd <- md_bayes_draw_lambda(toy, c(2,3), form )
#' # Function Prep #
#' # cnames <- c("lambda.black.canda", "lambda.black.candb")
#'
#' # Canda a over candb among black voters#
#' # lambda_two_compare(lmd, cnames=cnames, cand1or2 = 1)
#' }
#' @importFrom graphics abline hist par points
#' @importFrom stats median
#' @export lambda_two_compare
lambda_two_compare <- function(lmd, cnames, group_name = "Latino",
                               cand1or2 = 1) {
  if (cand1or2 == 2) {
    cnames <- rev(cnames)
    p_cand_over_cand <- lmd[, cnames[2]] - lmd[, cnames[1]]
  }

  n <- nrow(lmd)
  labels <- sub(".*\\.", "", cnames)

  # what is probability that Morales is preferred to Rothman among Latinos?
  p_cand_over_cand <- lmd[, cnames[1]] - lmd[, cnames[2]]

  # Plot Histogram & Posterior #
  graphics::par(mfrow = c(1, 2))

  # Histogram #
  graphics::hist(lmd[, cnames[1]] - lmd[, cnames[2]],
    main = paste(labels[1], " - ", labels[2], sep = ""),
    xlab = "Posterior Distribution"
  )

  # density
  # Need regular expression for everything to right of period to extract names
  dens <- stats::density(p_cand_over_cand)
  plot(dens,
    main = paste("Difference in Proportion ", group_name,
      "\nVote for ", labels[1], " and ", labels[2],
      sep = ""
    ),
    xlab = "Difference in Posterior Distribution Sampled from "
  )
  # plot individual posterior sampled points (no)
  graphics::points(p_cand_over_cand, rep(0, length(p_cand_over_cand)), pch = 3)
  graphics::abline(v = 0, col = "grey", lty = 2)

  # Probability 1 Candidate over Another #
  # Morales over Rothman by at least 10 points
  c1g10 <- length(which(p_cand_over_cand > .10)) / n
  # Morales over Rothman by at least 5 points
  c1g5 <- length(which(p_cand_over_cand > .05)) / n
  c1g0 <- length(which(p_cand_over_cand > 0)) / n

  med <- stats::median(p_cand_over_cand)
  mean <- mean(p_cand_over_cand)
  df <- data.frame(c1g10, c1g5, c1g0, round(med, 3), round(mean, 3))
  colnames(df) <- c(
    "Prob>10%",
    "Prob>5%",
    "Prob>0",
    "Dist. Median",
    "Dist. Mean"
  )
  return(df)
}
