#' Class \code{"ei_compare"}
#'
#' An S4 class object stemming from ei_rc_good_table(), used for plotting, and
#' examining comparison results.
#'
#'
#' @name ei_compare-class
#' @docType class
#' @section Objects from the Class: Objects can, in principle, be created by
#' calls of the form \code{new("ei_compare", ...)}.  However, the preferred
#' form is to have them called ei_rc_good_table()
#' @author Loren Collingwood <loren.collingwood@@ucr.edu>
#' @keywords classes
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
#' ei_bayes_res <- bayes_table_make(ei_bayes, cand_vector = cands, table_names = table_names)
#' ei_bayes_res <- ei_bayes_res[c(1, 2, 5), ]
#' # Combine Results, results in object of class ei_compare
#' ei_rc_combine <- ei_rc_good_table(results, ei_bayes_res,
#'   groups = c("Black", "White")
#' )
#' # Produces data and character vector, which can be sent to plot()
#' ei_rc_combine
#' }
#' \dontrun{
#' # Warning: Takes a while to run
#' # Load corona data
#' data(corona)
#' # Generate character vectors
#' cands <- c("pct_husted", "pct_spiegel", "pct_ruth", "pct_button", "pct_montanez", "pct_fox")
#' race_group3 <- c("~ pct_hisp", "~ pct_asian", "~ pct_white")
#' table_names <- c("EI: Pct Lat", "EI: Pct Asian", "EI: Pct White")
#' # Run EI iterative Fitting
#' results <- ei_est_gen(
#'   cand_vector = cands, race_group = race_group3,
#'   total = "totvote", data = corona, table_names = table_names
#' )
#'
#' # EI: RxC model
#' # Generate formula
#' form <- formula(cbind(pct_husted, pct_spiegel, pct_ruth, pct_button, pct_montanez, pct_fox)
#' ~ cbind(pct_hisp, pct_asian, pct_white))
#' ei_bayes <- ei.reg.bayes(form, data = corona, sample = 10000, truncate = TRUE)
#' # RxC table names
#' table_names <- c("RxC: Pct Hisp", "RxC: Pct Asian", "RxC: Pct White")
#' # Table Creation, using function bayes_table_make in ei_est_generalize.R file
#' ei_bayes_res <- bayes_table_make(ei_bayes, cand_vector = cands, table_names = table_names)
#'
#'
#' # Combine Results, results in object of class ei_compare
#' ei_rc_combine <- ei_rc_good_table(results, ei_bayes_res,
#'   groups = c("Latino", "Asian", "White")
#' )
#' # Produces data and character vector, which can be sent to plot()
#' ei_rc_combine
#' }
#'
NULL


#' Compares EI, Goodman, RxC Estimates
#'
#' Compares estimates from three ecological inferences routines, based on King
#' et. al.'s approach.
#'
#' See demo(demo, "eiCompare") for examples on how to use code
#'
#' @name eiCompare-package
#' @aliases eiCompare-package eiCompare
#' @docType package
#' @author Loren Collingwood
#'
#' Maintainer: Loren Collingwood <loren.collingwood@@ucr.edu>
#' @references Gary King (1997). A Solution to the Ecological Inference
#' Problem. Princeton: Princeton University Press. Lau, Olivia, Ryan Moore, and
#' Michael Kellerman. eiPack: Ecological Inference and Higher-Dimension Data
#' Management
#' @keywords package
NULL
