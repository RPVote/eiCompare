#' Iterative EI Estimation
#'
#' Iteratively fits EI models for candidates and racial/ethnic groups
#'
#' @param data A data.frame object containing turnout information by
#' candidate and race
#' @param cand_cols A character vector of the names of columns containing
#' data on turnout for each candidate
#' @param race_cols A character vector of the names of columns containing
#' data on turnout by race group.
#' @param totals_col The name of the column containing vote totals for each
#' row
#' @param rho The rho parameter for ei() estimate, defaults to 10, numeric
#' @param table_names A character vector of table names with same length as
#' race_cols. Used for formatting output. If only one racial group, must
#' provide "Pct. Other" as second element of vector
#' @param sample The number of samples used for EI calculation, default = 1000
#' @param tomog A logical to display tomography plot. If true will will save pdf
#' plot to working directory. Default is FALSE
#' @param density_plot A logical to display density plot of betab and betaw. If
#' true will save pdf plot to working directory. Default is FALSE
#' @param beta_yes A logical to export betas (b, w) in list object in addition to
#' table of results. Default is FALSE
#' @param verbose A logical to print messages, ei summaries as iterations progress.
#' @param \dots Arguments passed onto ei() function
#' @return Data frame/table object containing EI individually estimated
#' results. If beta_yes=TRUE, two list items, first the data frame table of
#' results, second dataframe of betas themselves.
#' @note If this results in an error, "Error in .subset2(x, i, exact = exact) :
#' invalid subscript type 'list'", just rerun the algorithm again.
#' @author Loren Collingwood <loren.collingwood@@ucr.edu>
#' @references eiPack. Gary King (1997). A Solution to the Ecological Inference
#' Problem. Princeton: Princeton University Press.
#' @examples
#'
#' # TOY DATA EXAMPLE
#' canda <- c(.1, .09, .85, .9, .92)
#' candb <- 1 - canda
#' white <- c(.8, .9, .10, .08, .11)
#' black <- 1 - white
#' total <- c(30, 80, 70, 20, 29)
#' toy <- data.frame(canda, candb, white, black, total)
#'
#' # CREATE VECTORS
#' cand_cols <- c("canda")
#' race_cols <- c("black") # only use one group for example
#' table_names <- c("EI: PCT Black", "EI: PCT White")
#'
#' # RUN ei_est_gen()
#' # KEEP DATA TO JUST ONE ROW FOR EXAMPLE (time) ONLY!
#' ei_est_gen(
#'   data = toy[c(1, 3, 5), ],
#'   cand_cols = cands,
#'   race_cols = race_group,
#'   totals_col = "total",
#'   table_names = table_names,
#'   sample = 100
#' )
#' \donttest{
#' # WARNING -- May take a little while to execute
#' # Load Package Data
#' data(corona)
#' # Create Character Vectors
#' cands <- c("pct_husted", "pct_spiegel", "pct_ruth", "pct_button", "pct_montanez", "pct_fox")
#' race_group3 <- c("pct_hisp", "pct_asian", "pct_white")
#' table_names <- c("EI: Pct Hisp", "EI: Pct Asian", "EI: Pct White")
#'
#' # Run ei_est_gen function
#' results <- ei_est_gen(
#'   data = corona,
#'   cand_cols = cands,
#'   race_group = race_group3,
#'   total = "totvote",
#'   table_names = table_names
#' )
#'
#' results
#' # Run ei_est_gen function; Exporting betas into data frame
#' results_w_betas <- ei_est_gen(
#'   data = corona,
#'   cand_cols = cands,
#'   race_group = race_group3,
#'   totals_col = "totvote",
#'   table_names = table_names,
#'   beta_yes = TRUE
#' )
#'
#' res1 <- results_w_betas[[1]] # table of mean estimates
#' res1
#' res2 <- results_w_betas[[2]] # betas of estimates for each precinct
#' }
#'
#' @import ei
#' @importFrom stats formula
#' @importFrom stringr str_trim
#' @importFrom grDevices pdf
#' @importFrom graphics mtext
#'
#'
#' @export
ei_est_gen <- function(
                       data,
                       cand_cols,
                       race_cols,
                       totals_col,
                       rho = 10,
                       table_names,
                       sample = 1000,
                       tomog = FALSE,
                       density_plot = FALSE,
                       beta_yes = FALSE,
                       verbose = FALSE,
                       ...) {
  list_extract <- function(x) x[, 1:2]
  seq_split <- 2:length(cand_cols)
  if (length(cand_cols) == 1) {
    rn <- c(cand_cols, "se")
  }
  else {
    rn <- c(R.utils::insert(cand_cols, ats = seq_split, values = rep("se", length(cand_cols) - 1)), "se")
  }
  data <- na.omit(data)
  race_cols_table <- list()
  beta_full_hold <- list()

  # add progress bar to show progress when verbose = FALSE
  if (!verbose) {
    pb <- txtProgressBar(min = 0, max = length(cand_cols) * length(race_cols), style = 3)
    j <- 0
  }

  for (k in 1:length(race_cols)) {
    cand_table <- list()
    beta_container <- list()
    for (i in 1:length(cand_cols)) {
      form <- stats::formula(paste(cand_cols[i], " ~ ", race_cols[k]))
      try(
        # if versbose == F, supress all output from ei::ei
        if (!verbose) {
          capture.output({
            suppressMessages({
              ei_out <- ei::ei(
                form,
                total = totals_col, erho = rho,
                data = data, sample = sample, ...
              )
            })
          })
        } else {
          ei_out <- ei::ei(
            form,
            total = totals_col, erho = rho,
            data = data, sample = sample, ...
          )
        },
        silent = T
      )
      gm <- geterrmessage()
      if (gm == "Maximizing likelihood\\n         Error in .subset2(x, i, exact = exact) : invalid subscript type 'list'") {
        stop("Maximizing likelihood\\n             Error in .subset2(x, i, exact = exact) : invalid subscript type 'list'\\n\\n             \\n ei package error try re-running ei_est_gen()")
      }
      if (verbose) {
        cat(paste("Model:", cand_cols[i], race_cols[k],
          "\\n",
          sep = " "
        ))
        print(summary(ei_out))
      }
      if (tomog) {
        grDevices::pdf(paste(cand_cols[i], race_cols[k], ".pdf",
          sep = ""
        ))
        plot(ei_out, "tomogE")
        graphics::mtext(paste(cand_cols[i], race_cols[k], sep = " "),
          outer = T, line = -1
        )
        # imguR::dev.off() #not required?
      }
      if (density_plot) {
        grDevices::pdf(paste("density_plot", k, i, ".pdf", sep = "_"))
        plot(ei_out, "betab", "betaw")
        graphics::mtext(paste(cand_cols[i], race_cols[k], sep = " "),
          outer = T, line = -1
        )
        # imguR::dev.off()
      }
      beta_stan_err <- ei::eiread(
        ei_out, "betab", "sbetab",
        "betaw", "sbetaw"
      )
      min_b <- summary(ei_out)$`Estimates of Aggregate Quantities of Interest`[1, 1] * 100
      min_ste <- summary(ei_out)$`Estimates of Aggregate Quantities of Interest`[1, 2] * 100
      non_b <- summary(ei_out)$`Estimates of Aggregate Quantities of Interest`[2, 1] * 100
      non_ste <- summary(ei_out)$`Estimates of Aggregate Quantities of Interest`[2, 2] * 100

      #  Weighted Mean #
      eimean <- data.frame(c(min_b, min_ste), c(
        non_b,
        non_ste
      ))
      cand_betas <- cbind(unlist(beta_stan_err[1]), unlist(beta_stan_err[3]))
      colnames(cand_betas) <- c("betab", "betaw")
      cand_table[[i]] <- eimean
      beta_container[[i]] <- cand_betas

      if (!verbose) {
        j <- j + 1
        setTxtProgressBar(pb, j)
      }
    }
    cand_table <- data.table::rbindlist(cand_table)
    cand_table <- data.frame(rn, cand_table)
    race_cols_table[[k]] <- cand_table
    beta_full_hold[[k]] <- beta_container
  }
  if (length(race_cols) == 1) {
    race_cols_table <- data.frame(race_cols_table)
    beta_full_hold <- data.frame(beta_full_hold)
    colnames(beta_full_hold) <- c("betab", "betaw")
  }
  else {
    race_cols_table <- data.frame(lapply(
      race_cols_table,
      list_extract
    ))
    race_cols_table <- race_cols_table[, c(1, seq(
      2, ncol(race_cols_table),
      2
    ))]
  }
  tot <- colSums(race_cols_table[seq(
    1, nrow(race_cols_table),
    2
  ), 2:ncol(race_cols_table)])
  just_data <- race_cols_table[, 2:ncol(race_cols_table)]
  add <- rbind(just_data, tot)
  add <- data.frame(1:nrow(add), add)
  colnames(add) <- c("Candidate", table_names)
  add[, 1] <- c(as.character(race_cols_table[, 1]), "Total")
  race_cols_table <- add
  if (beta_yes) {
    beta_names <- list()
    for (i in 1:length(race_cols)) {
      beta_names[[i]] <- paste(stringr::str_trim(gsub("~", "", race_cols[i])),
        cand_cols,
        sep = "_"
      )
    }
    beta_names <- as.vector(unlist(beta_names))
    beta_b <- paste("betab", beta_names, sep = "_")
    beta_w <- paste("betaw", beta_names, sep = "_")
    beta_names <- R.utils::insert(beta_b, ats = 1:length(beta_b) +
      1, values = beta_w)
    beta_full_hold <- as.data.frame(beta_full_hold)
    names(beta_full_hold) <- beta_names
    return(list(race_cols_table = race_cols_table, all_betas = beta_full_hold))
  }
  else {
    return(race_cols_table)
  }
}
