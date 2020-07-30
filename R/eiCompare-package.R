

#' Corona 2006
#'
#' Precinct vote data from a Corona, CA 2006 election
#'
#'
#' @name cor_06
#' @docType data
#' @format A data frame with 47 observations on the following 8 variables.
#' \describe{ \item{list("precinct")}{a numeric vector}
#' \item{list("totvote")}{a numeric vector} \item{list("pct_latino")}{a numeric
#' vector} \item{list("pct_other")}{a numeric vector}
#' \item{list("pct_breitenbucher")}{a numeric vector}
#' \item{list("pct_montanez")}{a numeric vector} \item{list("pct_spiegel")}{a
#' numeric vector} \item{list("pct_skipworth")}{a numeric vector} }
#' @references Riverside County, CA board of elections
#' @keywords datasets
#' @examples
#'
#' data(cor_06)
#' # Look at data
#' head(cor_06)
#' str(cor_06)
NULL





#' Corona Precinct Racial Bloc Voting Data
#'
#' Data taken from a 2014 California election, with precinct results and racial
#' demographics for Corona, CA precincts
#'
#'
#' @name corona
#' @docType data
#' @format A data frame with 46 observations on the following 12 variables.
#' \describe{ \item{list("precinct")}{a numeric vector}
#' \item{list("totvote")}{a numeric vector} \item{list("pct_husted")}{a numeric
#' vector} \item{list("pct_spiegel")}{a numeric vector}
#' \item{list("pct_ruth")}{a numeric vector} \item{list("pct_button")}{a
#' numeric vector} \item{list("pct_montanez")}{a numeric vector}
#' \item{list("pct_fox")}{a numeric vector} \item{list("pct_hisp")}{a numeric
#' vector} \item{list("pct_asian")}{a numeric vector}
#' \item{list("pct_white")}{a numeric vector} \item{list("pct_non_lat")}{a
#' numeric vector} }
#' @references Riverside County, CA board of elections
#' @keywords datasets
#' @examples
#'
#' data(corona)
#' head(corona)
#' str(corona)
NULL





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





#' Los Angeles County Data
#'
#' Los Angeles County precinct dataset from 2010 election
#'
#'
#' @name lac_10
#' @docType data
#' @format A data frame with 4980 observations on the following 10 variables.
#' \describe{ \item{list("precinct")}{a character vector}
#' \item{list("tot_reg")}{a numeric vector} \item{list("i_jones")}{a numeric
#' vector} \item{list("i_delatore")}{a numeric vector}
#' \item{list("votescast")}{a numeric vector} \item{list("lat_voters")}{a
#' numeric vector} \item{list("pct_latino")}{a numeric vector}
#' \item{list("pct_delatorre")}{a numeric vector} \item{list("pct_jones")}{a
#' numeric vector} \item{list("pct_other")}{a numeric vector} }
#' @source Los Angeles county
#' @keywords datasets
#' @examples
#'
#' data(lac_10)
#' head(lac_10)
#' str(lac_10)
NULL





#' New York state FIPS codes
#'
#' New York state FIPS codes example of 500 voters
#'
#'
#' @name ny_fips
#' @docType data
#' @format A data frame with 500 observations on the following 2 variables.
#' \describe{ \item{list("row_id")}{unique id} \item{list("FIP")}{15 digit FIPS
#' code including state, county, tract, block} }
#' @source FCC
#' @keywords datasets
#' @examples
#'
#' data(ny_fips)
#' str(ny_fips)
NULL





#' New York voter file sample
#'
#' New York voter file sample; example of 500 voters
#'
#'
#' @name ny_voter
#' @docType data
#' @format A data frame with 500 observations on the following 10 variables.
#' \describe{ \item{list("Voter.ID")}{Unique voter file id number, jumbled}
#' \item{list("SD..Poll")}{Precinct id} \item{list("fips")}{15-digit fips code}
#' \item{list("st")}{state fips code} \item{list("county")}{county fips code}
#' \item{list("tract")}{tract fips code} \item{list("block")}{block fips code}
#' \item{list("st_cty")}{state county fips code}
#' \item{list("st_cty_tract")}{state county tract fips code}
#' \item{list("Last.Name")}{Voters' surname} }
#' @source ERCSD board of election
#' @keywords datasets
#' @examples
#'
#' data(ny_voter)
#' str(ny_voter)
NULL
