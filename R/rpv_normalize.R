#' @export
#'
#' @author Rachel Carroll <rachelcarroll4@@gmail.com>
#' @author Loren Collingwood <lcollingwood@@unm.edu>
#'
#' @title Normalize RPV results
#'
#' @description Create a dataframe of normalized RPV results when using the
#' cvap, vap, or bisg denominator method, i.e., take RPV results only among
#' people estimated to have voted.
#'
#' @param ei_object Output from \code{ei_iter()} or \code{ei_rxc()}
#' @param cand_cols A character vector of the candidate column names to be
#' normalized from \code{ei_object}. Only use candidate column name columns,
#' not the No Vote column.
#' @param race_cols A character vector of the racial group column names to
#' be normalized from \code{ei_object}
#'
#' @return Normalized RPV results in a data.frame
#' @examples
#' \donttest{
#' # library(eiCompare)
#' # data("south_carolina")
#' # prec_election_demog <- south_carolina[1:50,]
#'
#' ## run rpv using eiCompare rxc method
#' # rxcVote <-  ei_rxc(
#' #  data = prec_election_demog,
#' #  cand_cols = c('pct_mcmaster', 'pct_smith', 'pct_other_gov', 'pct_NoVote_gov'),
#' #  race_cols = c('pct_white', 'pct_black', 'pct_race_other'),
#' #  totals_col = "total_vap")
#'
#' ## normalize results accounting for no vote using rpv_normalize()
#' ## only include the candidate and race cols of interest for the rpv analysis
#' # rpv_results <- rpv_normalize(
#' # ei_object = rxcVote,
#' # cand_cols = c('pct_mcmaster', 'pct_smith', 'pct_other_gov'),
#' # race_cols = c('pct_white', 'pct_black')
#' # )
#' }
rpv_normalize <- function(ei_object, cand_cols, race_cols) {
  #----------------------------      QC     -----------------------------------#

  # Make sure ei_object is correct class
  if (!inherits(ei_object, "eiCompare")) {
    stop("ei_object must be an eiCompare output from ei_iter() or ei_rxc()")
  }
  # ei_good does not produce posterior samples needed for normalization
  if (!is.null(ei_object$type) && ei_object$type == "goodman") {
    stop("ei_good() output does not work with rpv_normalize(). Goodman's regression does not produce posterior district-level samples, which are required to compute normalized vote share estimates.")
  }
  # Make sure all cand_cols are in the ei_object
  canddiff <- setdiff(
    cand_cols,
    unique(ei_object$estimates$cand)
  )

  if (length(canddiff) > 0) {
    stop("cand_cols values are not found in ei_object")
  }

  # Make sure all race_cols are in the ei_object
  racediff <- setdiff(
    race_cols,
    unique(ei_object$estimates$race)
  )

  if (length(racediff) > 0) {
    stop("race_cols values are not found in ei_object")
  }

  #-----------------------     Helper Function    -----------------------------#
  # Create calculation helper function (used in lapply below)
  #     candNm = name of cand col in samplesDF (an element from cand_cols )
  #     raceNm = name of race/demographic samples contained in samplesDF (an
  #               elements from race_cols)
  #     samplesDF = data.frame with vote samples from eiCompare model for each
  #                 candidate in cand_cols and sums across all candidates. The samples
  #                 in samplesDF will be associated with a single race from race_cols

  sample_calcs <- function(candNm, raceNm, samplesDF) {
    # get share estimate
    share.est <- samplesDF %>%
      dplyr::pull(candNm) / samplesDF$sum

    # get average
    mean <- mean(share.est)
    # get lower and upper bounds
    quantile <- quantile(share.est, c(.025, .975))

    # compile results into a data.frame row
    row <- data.frame(
      "mean" = round(mean * 100, 2),
      "ci_95_lower" = round(quantile[1] * 100, 2),
      "ci_95_upper" = round(quantile[2] * 100, 2)
    )

    # set col and row names
    colnames(row) <- paste(raceNm, colnames(row), sep = ".")
    rownames(row) <- candNm

    # return results
    return(row)
  }

  #------------------   Calculate Normalized RPV Results    --------------------#

  # Initiate lists/vars
  samples <- list()
  results <- list()
  ncands <- length(cand_cols)

  # loop through race
  for (j in seq(length(race_cols))) {
    # store race name
    race_j_name <- race_cols[j]

    # get eiCompare model samples for each cand
    for (i in seq(ncands)) {
      # vote samples
      samples[[race_j_name]][[cand_cols[i]]] <-
        ei_object$district_samples[[paste(cand_cols[i], race_j_name, sep = "_")]]
    }

    # create df of samples of all cands for a given demographic preference
    race_j_samples <- samples[[j]]
    samplesDF <- dplyr::bind_cols(race_j_samples)
    # sum across all cand fields
    samplesDF$sum <- as.numeric(apply(samplesDF[, 1:ncands], 1, sum))

    # use helper function to get normalized point estimates and lower/upper bounds
    resultsList <- lapply(cand_cols, sample_calcs,
      raceNm = race_j_name, samplesDF = samplesDF
    )

    # store results
    results[[j]] <- dplyr::bind_rows(resultsList)
  }

  # combine results into one dataframe
  out <- dplyr::bind_cols(results)

  #------------------------    Print and Return    --------------------------#

  # return as object
  return(out)
}
