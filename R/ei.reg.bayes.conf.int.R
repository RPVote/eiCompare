#' Creates EI Reg Bayes Tables
#' 
#' Creates EI reg bayes tables with confidence bands
#' 
#' 
#' @param ei_bayes Object result of call to ei.reg.bayes() function.
#' @return Matrix object, table of results
#' @author Loren Collingwood <loren.collingwood@@ucr.edu>
#' @references eiPack, King et. al. (http://gking.harvard.edu/eiR)
#' @examples
#' 
#' 
#' # TOY DATA EXAMPLE
#' canda <- c(.1, .09, .85, .9, .92)
#' candb <- 1-canda
#' white <- c(.8, .9, .10, .08, .11)
#' black <- 1 - white
#' total <- c(30,80, 70, 20, 29)
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
#'                       data = toy[c(1,3,5),], table_names = table_names, sample=100)
#' 
#' # Generate formula for passage to ei.reg.bayes() function
#' form <- formula(cbind(canda,candb) ~ cbind(black, white)) 
#' # Run Bayesian model
#' suppressWarnings (
#'   ei_bayes <- ei.reg.bayes(form, data=toy, sample=100, truncate=TRUE)
#' )
#' 
#' # Produce Table
#' ei.reg.bayes.conf.int(ei_bayes)
#' 
#' 
#' \donttest{
#' # Warning: Takes a while to run
#' # Load corona data
#' data(corona)
#' # Generate character vectors
#' cands <- c("pct_husted","pct_spiegel","pct_ruth","pct_button","pct_montanez","pct_fox")
#' race_group3 <- c("~ pct_hisp", "~ pct_asian", "~ pct_white")
#' table_names <- c("EI: Pct Lat", "EI: Pct Asian", "EI: Pct White")
#' # Run EI iterative Fitting
#' results <- ei_est_gen(cand_vector=cands, race_group = race_group3,
#' 			total = "totvote", data = corona, table_names = table_names)
#' 
#' # EI: RxC model 
#' # Generate formula
#' form <- formula(cbind(pct_husted,pct_spiegel,pct_ruth,pct_button,pct_montanez,pct_fox) 
#' ~ cbind(pct_hisp, pct_asian, pct_white)) 
#' suppressWarnings (
#' ei_bayes <- ei.reg.bayes(form, data=corona, sample=10000, truncate=TRUE)
#' )
#' # Produce Table
#' ei.reg.bayes.conf.int(ei_bayes)
#' 
#' }
#' 
#' @export ei.reg.bayes.conf.int
ei.reg.bayes.conf.int <- function(ei_bayes) {
  conf_int <- apply(ei_bayes$draws, 1:2, function(x) quantile(x, probs = c(.025, .5, .975)))
  tab_out <- t(as.data.frame(conf_int))
  return(tab_out)
}
