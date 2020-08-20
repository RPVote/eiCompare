#' Iterative EI Estimation
#'
#' Iteratively fits EI models for candidates and racial/ethnic groups
#'
#'
#' @param cand_vector Character vector of candidate names, taken from the
#' dataset
#' @param race_group Character vector of formula, e.g., "~ pct_latino"
#' @param total Character vector (e.g., "totvote") of total variable name from
#' data, variable in data is numeric
#' @param rho Rho parameter for ei() estimate, defaults to 10, numeric
#' @param data data.frame() object containing the data
#' @param table_names Character vector of table names with same length as
#' race_group. Used for formatting output. If only one racial group, must
#' provide "Pct. Other" as second element of vector
#' @param sample Number of samples used for EI calculation, default = 1000
#' @param tomog Logical to display tomography plot. If true will will save pdf
#' plot to working directory. Default is FALSE
#' @param density_plot Logical to display density plot of betab and betaw. If
#' true will save pdf plot to working directory. Default is FALSE
#' @param seed An integer seed value for replicating estimate results across
#' runs. If NULL, a random seed is chosen. Defaulted to NULL.
#' @param beta_yes Logical to export betas (b, w) in list object in addition to
#' table of results. Default is FALSE
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
#' \dontrun{
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
#' ei_est_gen(cands, race_group, "total",
#'   data = toy[c(1, 3, 5), ], table_names = table_names, sample = 100
#' )
#'
#' # WARNING -- May take a little while to execute
#' # Load Package Data
#' data(corona)
#' # Create Character Vectors
#' cands <- c("pct_husted", "pct_spiegel", "pct_ruth", "pct_button", "pct_montanez", "pct_fox")
#' race_group3 <- c("~ pct_hisp", "~ pct_asian", "~ pct_white")
#' table_names <- c("EI: Pct Hisp", "EI: Pct Asian", "EI: Pct White")
#'
#' # Run ei_est_gen function
#' results <- ei_est_gen(
#'   cand_vector = cands, race_group = race_group3,
#'   total = "totvote", data = corona, table_names = table_names
#' )
#'
#' results
#' # Run ei_est_gen function; Exporting betas into data frame
#' results_w_betas <- ei_est_gen(
#'   cand_vector = cands, race_group = race_group3,
#'   total = "totvote", data = corona, table_names = table_names, beta_yes = TRUE
#' )
#'
#' res1 <- results_w_betas[[1]] # table of mean estimates
#' res1
#' res2 <- results_w_betas[[2]] # betas of estimates for each precinct
#' }
#'
#' @import ei
#' @importFrom stringr str_trim
#' @importFrom stats formula
#' @importFrom grDevices pdf
#' @importFrom graphics mtext
#'
#'
#' @export
ei_est_gen <- function(cand_vector,
                       race_group,
                       total,
                       rho = 10,
                       data,
                       table_names,
                       sample = 1000,
                       tomog = F,
                       density_plot = F,
                       beta_yes = F,
                       seed = NULL,
                       ...) {
  list_extract <- function(x) x[, 1:2]
  seq_split <- 2:length(cand_vector)
  if (length(cand_vector) == 1) {
    rn <- c(cand_vector, "se")
  }
  else {
    se_cols <- rep("se", length(cand_vector))
    rn <- c(rbind(cand_vector, se_cols))
  }
  data <- na.omit(data)
  race_group_table <- list()
  beta_full_hold <- list()
  for (k in 1:length(race_group)) {
    cand_table <- list()
    beta_container <- list()
    for (i in 1:length(cand_vector)) {
      form <- stats::formula(paste(cand_vector[i], race_group[k]))
      if (!is.null(seed)) set.seed(seed)
      try(ei_out <- ei::ei(form,
        total = total, erho = rho,
        data = data, sample = sample, ...
      ), silent = T)
      gm <- geterrmessage()
      if (gm == "Maximizing likelihood\\n         Error in .subset2(x, i, exact = exact) : invalid subscript type 'list'") {
        stop("Maximizing likelihood\\n             Error in .subset2(x, i, exact = exact) : invalid subscript type 'list'\\n\\n             \\n ei package error try re-running ei_est_gen()")
      }
      cat(paste("Model:", cand_vector[i], race_group[k],
        "\\n",
        sep = " "
      ))
      print(summary(ei_out))
      if (tomog) {
        grDevices::pdf(paste(cand_vector[i], race_group[k], ".pdf",
          sep = ""
        ))
        plot(ei_out, "tomogE")
        graphics::mtext(paste(cand_vector[i], race_group[k], sep = " "),
          outer = T, line = -1
        )
        # imguR::dev.off() #not required?
      }
      if (density_plot) {
        grDevices::pdf(paste("density_plot", k, i, ".pdf", sep = "_"))
        plot(ei_out, "betab", "betaw")
        graphics::mtext(paste(cand_vector[i], race_group[k], sep = " "),
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
    }
    cand_table <- data.table::rbindlist(cand_table)
    cand_table <- data.frame(rn, cand_table)
    race_group_table[[k]] <- cand_table
    beta_full_hold[[k]] <- beta_container
  }
  if (length(race_group) == 1) {
    race_group_table <- data.frame(race_group_table)
    beta_full_hold <- data.frame(beta_full_hold)
    colnames(beta_full_hold) <- c("betab", "betaw")
  }
  else {
    race_group_table <- data.frame(lapply(
      race_group_table,
      list_extract
    ))
    race_group_table <- race_group_table[, c(1, seq(
      2, ncol(race_group_table),
      2
    ))]
  }
  tot <- colSums(race_group_table[seq(
    1, nrow(race_group_table),
    2
  ), 2:ncol(race_group_table)])
  just_data <- race_group_table[, 2:ncol(race_group_table)]
  add <- rbind(just_data, tot)
  add <- data.frame(1:nrow(add), add)
  colnames(add) <- c("Candidate", table_names)
  add[, 1] <- c(as.character(race_group_table[, 1]), "Total")
  race_group_table <- add
  if (beta_yes) {
    beta_names <- list()
    for (i in 1:length(race_group)) {
      beta_names[[i]] <- paste(stringr::str_trim(gsub("~", "", race_group[i])),
        cand_vector,
        sep = "_"
      )
    }
    beta_names <- as.vector(unlist(beta_names))
    beta_b <- paste("betab", beta_names, sep = "_")
    beta_w <- paste("betaw", beta_names, sep = "_")
    beta_names <- c(rbind(beta_b, beta_w))
    beta_full_hold <- as.data.frame(beta_full_hold)
    names(beta_full_hold) <- beta_names
    return(list(race_group_table = race_group_table, all_betas = beta_full_hold))
  }
  else {
    return(race_group_table)
  }
}
