#' EI iterative estimation via Goodman's Regression
#'
#' @author Loren Collingwood <loren.collingwood@@ucr.edu>
#' @author Ari Decter-Frain <agd75@@cornell.edu>
#'
#' @param data A data.frame() object containing precinct-level turnout data by
#' race and candidate
#' @param cand_cols A character vector listing the column names for turnout for
#' each candidate
#' @param race_cols A character vector listing the column names for turnout by
#' race
#' @param totals_col The name of the column containing total votes cast in each
#' precinct
#' @return An object of class eiCompare containing Goodman's regression estimates
#' @references eiPack King et. al. (https://gking.harvard.edu/eiR/)
#'
#' L. A. Goodman. Ecological regressions and behavior of individuals. American
#' Sociological Review, 1953.
#'
#' @importFrom stats formula glm coef na.omit
#' @export
ei_good <- function(data,
                    cand_cols,
                    race_cols,
                    totals_col) {
  # Check for valid arguments
  check_args(data, cand_cols, race_cols, totals_col)

  # Subset data
  data <- data[, c(cand_cols, race_cols, totals_col)]

  # Check for missings
  data <- remove_nas(data)

  # Force data to be a dataframe
  data <- as.data.frame(data)

  # Get race and cand lengths
  n_races <- length(race_cols)
  n_cands <- length(cand_cols)
  n_iters <- n_races * n_cands

  # Create race-cand pairs for iteration
  race_cand_pairs <- expand.grid(
    "race" = race_cols,
    "cand" = cand_cols,
    stringsAsFactors = FALSE
  )

  # Vectors for storing results
  vote_pcts <- c()
  std_errs <- c()
  cands_vec <- c()
  races_vec <- c()

  # Create count of estimates pushed to 0, 1 bounds
  bounded <- 0

  # Loop through each 2x2 ei
  for (i in 1:n_iters) {
    cand <- race_cand_pairs[i, "cand"]
    race <- race_cand_pairs[i, "race"]

    # Get formula for Goodman's Regression
    formula <- stats::formula(paste(cand, "~", race, "+", totals_col), sep = " ")

    # Estimate linear model
    res <- stats::glm(data = data, formula = formula)

    # Compute vote pct from coefficients
    vote_pct <- sum(stats::coef(res)[1:2])

    # Check if vote pct greater than 1 or less than 0
    if (vote_pct > 1.0) {
      bounded <- bounded + 1
      vote_pct <- 1.0
    } else if (vote_pct < 0) {
      bounded <- bounded + 1
      vote_pct <- 0
    }

    # Compute SE
    ses <- coef(summary(res))[, 2]

    # SE on sum of coefficients = sqrt(SE1^2 + SE2^2 + 2COV(1,2))
    # Omit the 2COV(1,2) term because covariance here is spurious
    se <- sqrt(ses[1]^2 + ses[2]^2)

    # Store results
    vote_pcts <- append(vote_pcts, vote_pct)
    std_errs <- append(std_errs, se)
    cands_vec <- append(cands_vec, cand)
    races_vec <- append(races_vec, race)
  }

  # Print warning if estimates were bounded
  if (bounded != 0) {
    warning(
      paste(
        bounded,
        "estimate(s) exceeded the (0,1) bounds. They have been forced down to the",
        "bounds"
      )
    )
  }

  # Build eiCompare class object
  estimates <- data.frame(
    cand = cands_vec,
    race = races_vec,
    mean = vote_pcts,
    sd = std_errs,
    stringsAsFactors = FALSE
  )

  output <- list(
    "type" = "goodman",
    "estimates" = estimates,
    "precinct_samples" = NULL,
    "stat_objects" = NULL,
    "name" = ""
  )
  class(output) <- "eiCompare"
  return(output)
}
