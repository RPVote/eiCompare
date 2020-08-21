#' Goodman Regression Generalization
#'
#' Makes summary table out of multiple heckman regression results, for multiple
#' candidates and groups
#'
#'
#' @param cand_vector Character vector of candidate names, taken from the
#' dataset
#' @param race_group Character vector of formula, e.g., "~ pct_latino"
#' @param total Character vector (e.g., "totvote") of total variable name from
#' data, variable in data is numeric
#' @param data data.frame() object containing the data
#' @param table_names Character vector of table names with same length as
#' race_group. Used for formatting output
#' @param \dots Arguments passed onto lm() function
#' @return Object of class data.frame() returned containing table summary of
#' all the Goodman regressions
#' @author Loren Collingwood <loren.collingwood@@ucr.edu>
#' @seealso \code{\link{ei_rc_good_table}}
#' @references eiPack King et. al. (http://gking.harvard.edu/eiR) L. A.
#' Goodman. Ecological regressions and behavior of individuals. American
#' Sociological Review, 1953.
#' @examples
#'
#' # Load corona data
#' \dontrun{
#' data(corona)
#' # Generate character vectors
#' cands <- c("pct_husted", "pct_spiegel", "pct_ruth", "pct_button", "pct_montanez", "pct_fox")
#' race_group3 <- c("~ pct_hisp", "~ pct_asian", "~ pct_white")
#'
#' # Goodman Regression
#' table_names <- c("Good: Pct Lat", "Good: Pct Asian", "Good: Pct Wht")
#' good_corona <- goodman_generalize(cands, race_group3, "totvote", corona, table_names)
#' }
#' @importFrom stats formula lm coef na.omit
#'
#' @export goodman_generalize
goodman_generalize <-
  function(cand_vector, race_group, total, data, table_names, ...) {

    # Functions
    list_extract <- function(x) x[, 1:2] # sends to lapply to extract indiv column estimates

    # Table/Output Row Labeling
    se_cols <- rep("se", length(cand_vector))
    rn <- c(rbind(cand_vector, se_cols))

    # Remove any missing datas
    data <- na.omit(data)

    # Loop Placeholder
    race_group_table <- list()

    # Loop over Race Vector
    for (k in 1:length(race_group)) {
      cand_table <- list()
      for (i in 1:length(cand_vector)) {
        form <- stats::formula(paste(cand_vector[i], race_group[k], "+", total))
        summary(res <- stats::lm(form, data = data, ...))
        vote_pct <- stats::coef(res)[1] + stats::coef(res)[2]
        ste <- stats::coef(summary(res))[, "Std. Error"]
        vote_ste <- ste[1] + ste[2]
        cand_table[[i]] <- c(vote_pct, vote_ste) * 100
      }

      cand_table <- unlist(cand_table) # cand_table is for one racial group and all candidates
      cand_table <- data.frame(rn, cand_table) # Add in vector for labeling
      race_group_table[[k]] <- cand_table # Put candidate results into list
    }

    if (length(race_group) == 1) { # For when there is just % Minority vs. % White, for example

      race_group_table <- data.frame(race_group_table)
    } else { # For when there are multiple groups (e.g., pct_hisp, pct_asian, pct_white
      race_group_table <- data.frame(race_group_table) # Turn list into data.frame
      race_group_table <- race_group_table[, c(1, seq(2, ncol(race_group_table), 2))] # clean up table
    }
    # Adding on Total Row
    tot <- colSums(race_group_table[seq(1, nrow(race_group_table), 2), 2:ncol(race_group_table)])
    just_data <- race_group_table[, 2:ncol(race_group_table)]
    add <- rbind(just_data, tot)
    add <- data.frame(1:nrow(add), add)
    colnames(add) <- c("Candidate", table_names)
    add[, 1] <- c(as.character(race_group_table[, 1]), "Total")
    race_group_table <- add

    return(race_group_table)
  }
