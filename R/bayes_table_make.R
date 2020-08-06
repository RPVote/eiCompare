#' EI:RxC Bayes Table Make
#'
#' Creates data.frame() table from eiPack RxC output, in the same format as
#' ei_est_gen.
#'
#' @param ei_bayes_object Output from eiPack ei.reg.bayes() function
#' @param cand_vector Character vector of candidate name variables, usually
#' "pct_johns" or something
#' @param table_names Character vector of column names, e.g., c("RxC: Pct
#' Hisp", "RxC: Pct Asian")
#' @return Data frame object in similar vein to ei_est_gen
#' @author Loren Collingwood <loren.collingwood@@ucr.edu>
#' @references O. Lau, R. T. Moore, and M. Kellermann. eipack: RxC ecological
#' inference and higher-dimension data management. New Functions for
#' Multivariate Analysis, 18(1):43, 2006.
#' @examples
#'
#' # TOY DATA EXAMPLE
#' \dontrun{
#' canda <- runif(5)
#' candb <- 1 - canda
#' white <- runif(5)
#' black <- 1 - white
#' total <- round(runif(5, min = 20, max = 40), 0)
#'
#' toy <- data.frame(canda, candb, white, black, total)
#'
#' cands <- c("canda", "candb")
#' table_names <- c("RxC: PCT Black", "RxC PCT White")
#'
#' # generate formula for passage to ei.reg.bayes() function
#' form <- formula(cbind(canda, candb) ~ cbind(black, white))
#' # run bayesian model
#' suppressWarnings(
#'   ei_bayes <- ei.reg.bayes(form, data = toy, sample = 100, truncate = TRUE)
#' )
#' # table creation, using function bayes_table_make
#' ei_bayes_res <- bayes_table_make(ei_bayes,
#'   cand_vector = cands,
#'   table_names = table_names
#' )
#' ei_bayes_res
#' # load package data
#' data(corona)
#' # create character vectors
#' cands <- c(
#'   "pct_husted",
#'   "pct_spiegel",
#'   "pct_ruth",
#'   "pct_button",
#'   "pct_montanez",
#'   "pct_fox"
#' )
#' table_names <- c("RxC: Pct Hisp", "RxC: Pct Asian", "RxC: Pct White")
#'
#' # generate formula for passage to ei.reg.bayes() function
#' form <- formula(cbind(
#'   pct_husted, pct_spiegel, pct_ruth,
#'   pct_button, pct_montanez, pct_fox
#' )
#' ~ cbind(pct_hisp, pct_asian, pct_white))
#' # run bayesian model
#' suppressWarnings(
#'   ei_bayes <- ei.reg.bayes(
#'     form,
#'     data = corona,
#'     sample = 10000,
#'     truncate = TRUE
#'   )
#' )
#' # table creation using bayes_table_make
#' ei_bayes_res <- bayes_table_make(ei_bayes,
#'   cand_vector = cands,
#'   table_names = table_names
#' )
#' ei_bayes_res
#' }
#'
#' @importFrom R.utils insert
#' @importFrom data.table rbindlist
#' @export bayes_table_make
bayes_table_make <-
  function(ei_bayes_object, cand_vector, table_names) {

    # Used for Later Sorting/Colnames
    seq_split <- 2:length(cand_vector)
    rn <- c(
      R.utils::insert(cand_vector,
        ats = seq_split,
        values = rep("se", length(cand_vector) - 1)
      ),
      "se"
    )
    # Summarize Bayes Object to get posterior means/devs
    ei_bayes_object <- summary(ei_bayes_object)
    means <- ei_bayes_object$coef # get the estimates
    means <- data.frame(means[, "Mean"], means[, "Std. Dev."])
    means <- t(means) # Transpose it

    # Have to break apart the data to put in correct order
    list_holder <- list()

    for (i in seq_len(length(cand_vector))) {
      # use grep() to collect appropriate subsetted column names
      subs <- grep(cand_vector[i], colnames(means), value = T)
      # extract that data and put into list
      subs_data <- means[, subs]
      # need to put on same column names for rbind() later
      colnames(subs_data) <- table_names
      list_holder[[i]] <- subs_data
    }
    # put lists together into table
    out <- data.table::rbindlist(list_holder) * 100
    # add on column of names
    out <- data.frame(rn, out)

    # adding on total row
    tot <- colSums(out[seq(1, nrow(out), 2), 2:ncol(out)])
    just_data <- out[, 2:ncol(out)]
    add <- rbind(just_data, tot)
    add <- data.frame(seq_len(nrow(add)), add)
    colnames(add) <- c("Candidate", table_names)
    add[, 1] <- c(as.character(out[, 1]), "Total")
    out <- add

    return(out)
  }
