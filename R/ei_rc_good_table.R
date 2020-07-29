#' Create EI Comparison Table
#'
#' Takes output from EI model, EI RxC model, Goodman regression, and puts them
#' into a data frame table for useful analysis and comparison.
#'
#'
#' @param ei Table/data frame object result from ei_est_gen. This assumes
#' beta_yes=FALSE in ei_est_gen(). See example below for beta_yes=TRUE in
#' ei_est_gen().
#' @param rc Table/data frame from EI:RxC process from bayes_table_make()
#' @param good Table/data frame from Goodman regression, from
#' goodman_generalize(). Default is nothing
#' @param groups Character vector of voting blocks (e.g., c("Latino", "White"))
#' @param include_good Logical, default is FALSE, Set to TRUE if including a
#' Goodman table/data object
#' @return Object of class ei_compare containing a 1. data.frame() slot of
#' comparisons across the three models; 2. Character vector of group names used
#' for later plotting
#' @note Most of the time the user will not include the Goodman table, as they
#' are interested in the EI vs. EI:RxC comparison
#' @author Loren Collingwood <loren.collingwood@@ucr.edu>
#' @references eiPack, King et. al. (http://gking.harvard.edu/eiR)
#' @examples
#'
#' \dontrun{
#' # TOY DATA EXAMPLE
#' canda <- c(.1, .09, .85, .9, .92)
#' candb <- 1 - canda
#' white <- c(.8, .9, .10, .08, .11)
#' black <- 1 - white
#' total <- c(30, 80, 70, 20, 29)
#' toy <- data.frame(canda, candb, white, black, total)
#'
#' # CREATE VECTORS
#' cands <- c("canda")
#' race_group <- c("~ black") # only use one group for example
#' table_names <- c("EI: PCT Black", "EI: PCT White")
#'
#' # RUN ei_est_gen()
#' # KEEP DATA TO JUST ONE ROW FOR EXAMPLE (time) ONLY!
#' results <- ei_est_gen(cands, race_group, "total",
#'   data = toy[c(1, 3, 5), ], table_names = table_names, sample = 100
#' )
#'
#' # Generate formula for passage to ei.reg.bayes() function
#' form <- formula(cbind(canda, candb) ~ cbind(black, white))
#' # Run Bayesian model
#' suppressWarnings(
#'   ei_bayes <- ei.reg.bayes(form, data = toy, sample = 100, truncate = TRUE)
#' )
#'
#' table_names <- c("RxC: PCT Black", "RxC: PCT White")
#' cands <- c("canda", "candb")
#' ei_bayes_res <- bayes_table_make(ei_bayes,
#'   cand_vector = cands,
#'   table_names = table_names
#' )
#' ei_bayes_res <- ei_bayes_res[c(1, 2, 5), ]
#' # Combine Results, results in object of class ei_compare
#' ei_rc_combine <- ei_rc_good_table(results, ei_bayes_res,
#'   groups = c("Black", "White")
#' )
#' # Produces data and character vector, which can be sent to plot()
#' ei_rc_combine
#'
#' # Warning: Takes a while to run
#' # Load corona data
#' data(corona)
#' # Generate character vectors
#' cands <- c(
#'   "pct_husted",
#'   "pct_spiegel",
#'   "pct_ruth",
#'   "pct_button",
#'   "pct_montanez",
#'   "pct_fox"
#' )
#' race_group3 <- c("~ pct_hisp", "~ pct_asian", "~ pct_white")
#' table_names <- c("EI: Pct Lat", "EI: Pct Asian", "EI: Pct White")
#' # Run EI iterative Fitting
#' results <- ei_est_gen(
#'   cand_vector = cands,
#'   race_group = race_group3,
#'   total = "totvote",
#'   data = corona,
#'   table_names = table_names
#' )
#'
#' # EI: RxC model
#' # Generate formula
#' form <- formula(cbind(
#'   pct_husted,
#'   pct_spiegel,
#'   pct_ruth,
#'   pct_button,
#'   pct_montanez,
#'   pct_fox
#' ) ~ cbind(pct_hisp, pct_asian, pct_white))
#' suppressWarnings(
#'   ei_bayes <- ei.reg.bayes(form,
#'     data = corona,
#'     sample = 10000,
#'     truncate = TRUE
#'   )
#' )
#' # RxC table names
#' table_names <- c("RxC: Pct Hisp", "RxC: Pct Asian", "RxC: Pct White")
#' # Table Creation, using function bayes_table_make in ei_est_generalize.R file
#' ei_bayes_res <- bayes_table_make(ei_bayes,
#'   cand_vector = cands,
#'   table_names = table_names
#' )
#'
#'
#' # Combine Results, results in object of class ei_compare
#' ei_rc_combine <- ei_rc_good_table(results,
#'   ei_bayes_res,
#'   groups = c("Latino", "Asian", "White")
#' )
#' ei_rc_combine
#'
#' # If set beta_yes = TRUE in ei_est_gen():
#' ei_rc_combine2 <- ei_rc_good_table(results[[1]],
#'   ei_bayes_res,
#'   groups = c("Black", "White")
#' )
#' }
#'
#' @importFrom methods new
#' @export ei_rc_good_table
ei_rc_good_table <-
  function(ei, rc, good, groups, include_good = FALSE) {
    candidate <- ei[, 1]
    # number of racial/ethnic groups
    num_group <- ncol(ei) - 1 #-1 because subtract candidate column

    if (include_good) {
      # put results together that then needs to get broken apart
      comb_table <- cbind(ei, rc[, 2:ncol(rc)], good[, 2:ncol(good)])

      i <- 0
      tab_list <- list()
      nam <- list()
      # need to go up to ncol() this time because we start at two
      while (i <= ncol(ei) - 2) {
        tab_select <- seq(2 + i, ncol(comb_table), num_group)
        tab_reorg <- comb_table[, tab_select]
        tab_reorg[["EI_Diff"]] <- tab_reorg[, 2] - tab_reorg[, 1] # RxC - EI
        tab_reorg$EI_Diff[seq(2, length(tab_reorg$EI_Diff), 2)] <- NA
        i <- i + 1 # trick to get table into first list object
        tab_list[[i]] <- tab_reorg
        nam[[i]] <- names(tab_reorg)
      }
      tab_out <- data.frame(candidate, data.frame(tab_list))
      nam <- unlist(nam)
      colnames(tab_out)[2:ncol(tab_out)] <- nam
    } else {
      # put results together that then needs to get broken apart
      comb_table <- cbind(ei, rc[, 2:ncol(rc)])

      i <- 0
      tab_list <- list()
      nam <- list()
      # need to go up to ncol() this time because we start at two
      while (i <= ncol(ei) - 2) {
        tab_select <- seq(2 + i, ncol(comb_table), num_group)
        tab_reorg <- comb_table[, tab_select]
        tab_reorg[["EI_Diff"]] <- tab_reorg[, 2] - tab_reorg[, 1] # RxC - EI
        tab_reorg$EI_Diff[seq(2, length(tab_reorg$EI_Diff), 2)] <- NA
        # trick to get table into first list object
        i <- i + 1
        tab_list[[i]] <- tab_reorg
        nam[[i]] <- names(tab_reorg)
      }
      tab_out <- data.frame(candidate, data.frame(tab_list))
      nam <- unlist(nam)
      colnames(tab_out)[2:ncol(tab_out)] <- nam
    }
    # create class object of "ei_compare"
    tab_out <- methods::new("ei_compare", data = tab_out, groups = groups)

    return(tab_out)
  }
