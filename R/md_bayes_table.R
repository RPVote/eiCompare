#' MD Bayes Generalize Table Creation
#'
#' This, combined with md_bayes_gen() produces tables of results compatible
#' with EI table of results.
#'
#'
#' @param md_results Results object from md_bayes_gen() function.
#' @return Data.frame object of candidate (rows) and race (columns) RxC
#' results. This, combined with results from ei_est_gen() sends to the
#' ei_rc_good_table() function for combined table results and comparisons.
#' @author Loren Collingwood <loren.collingwood@@ucr.edu>
#' @references eiPack, King et. al. (http://gking.harvard.edu/eiR)
#' @examples
#'
#' \donttest{
#' # TOY DATA EXAMPLE
#' canda <- c(10, 8, 10, 4, 8)
#' candb <- 20 - canda
#' white <- c(15, 12, 18, 6, 10)
#' black <- 20 - white
#' toy <- data.frame(canda, candb, white, black)
#'
#' # Generate formula for passage to ei.reg.bayes() function
#' form <- formula(cbind(canda, candb) ~ cbind(black, white))
#'
#' # Then execute md_bayes_gen(); not run here due to time
#' res <- md_bayes_gen(
#'   toy,
#'   form,
#'   total_yes = FALSE,
#'   ntunes = 1,
#'   thin = 1,
#'   totaldraws = 100,
#'   sample = 10,
#'   burnin = 1,
#'   ci_true = FALSE
#' )
#' md_bayes_table(res)
#' }
#' @export md_bayes_table
md_bayes_table <- function(md_results) {
  if (paste(names(md_results), collapse = "") == "tabledraws") {
    md_results <- md_results$table
  }
  # create various iterator/indexing objects
  len <- nrow(md_results[[1]])
  rnames <- row.names(md_results[[1]])
  num_groups <- length(md_results)
  group_names <- names(md_results)

  seq_split <- 2:len

  # for tables with just one candidate
  if (len == 1) {
    rn <- c(rnames, "se", "Total")
  } else {
    # more than one candidate
    se_cols <- rep("se", length(rnames))
    rn <- c(rbind(rnames, se_cols), "Total")
  }

  # result NA matrix holder
  result_mat <- matrix(NA, ncol = num_groups, nrow = (len * 2) + 1)

  for (j in 1:num_groups) {
    n <- len * 2
    fill <- rep(NA, n)

    odd <- seq(1, n, 2) # odd
    even <- seq(2, n, 2) # odd

    fill[odd] <- md_results[[j]][, 1]
    fill[even] <- md_results[[j]][, 2]

    result_mat[, j] <- c(fill, sum(fill[odd]))
  }

  # label columns
  colnames(result_mat) <- paste("RxC", group_names, sep = "_")
  # combine into Dataframe for ei_rc_good_table() function
  result_mat <- data.frame(Candidate = rn, result_mat, stringsAsFactors = F)

  return(result_mat)
}
