#' Plot Method for class ei_compare
#'
#' Allows quick plotting, using plot() of EI vs EI:RxC differences. Produces
#' ggplot2 ouput, amazing.
#'
#' Limited amount of plotting flexibility. If user wants more flexibility
#' extract relevant objects from ei_rc_good_table() output and do your own
#' plotting!
#'
#' @param x Object of class ei_compare, from the ei_rc_good_table() function
#' @param \dots Arguments passed onto plot() and par()
#' @return ggplot2 graph output of EI and RxC differences
#' @author Loren Collingwood <loren.collingwood@@ucr.edu>; Sergio Garcia-Rios
#' <garcia.rios@@cornell.edu>
#' @references eiPack, King et. al. (http://gking.harvard.edu/eiR)
#' @examples
#'
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
#'
#' # PLOT EI DIFFERENCES
#' plot(ei_rc_combine)
#' \donttest{
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
#' # EI: RxC model
#' # Generate formula
#' form <- formula(cbind(pct_husted, pct_spiegel, pct_ruth, pct_button, pct_montanez, pct_fox)
#' ~ cbind(pct_hisp, pct_asian, pct_white))
#' suppressWarnings(
#'   ei_bayes <- ei.reg.bayes(form, data = corona, sample = 10000, truncate = TRUE)
#' )
#' # RxC table names
#' table_names <- c("RxC: Pct Hisp", "RxC: Pct Asian", "RxC: Pct White")
#' # Table Creation, using function bayes_table_make in ei_est_generalize.R file
#' ei_bayes_res <- bayes_table_make(ei_bayes, cand_vector = cands, table_names = table_names)
#'
#' # Goodman Regression
#' table_names <- c("Good: Pct Lat", "Good: Pct Asian", "Good: Pct Wht")
#' good_corona <- goodman_generalize(cands, race_group3, "totvote", corona, table_names)
#'
#' # Combine Results, results in object of class ei_compare
#' ei_rc_g_combine <- ei_rc_good_table(results, ei_bayes_res, good_corona,
#'   groups = c("Latino", "Asian", "White")
#' )
#' # Plot the Results
#' plot(ei_rc_g_combine)
#' }
#'
#' @importFrom photobiology na.omit
#' @import ggplot2
#'
#'
#'
#' @export plot.ei_compare
plot.ei_compare <- function(x, ...) {

  # Calculate Number of Plots to Create
  group_num <- nplots <- length(x@groups) # Up to 4 groups!
  # Warnings
  if (group_num > 4) stop("Number of groups must be four or fewer booyah!") # Stop plot() if more than four racial/ethnic groups (for plotting purposes)
  if (!is.na(x@data[2, 4])) stop("In 'ei_rc_good_table()' function set include_good=F, I'm cereal you guys")

  # Collect Candidate Names for Labeling
  ei_rc_combine_a <- photobiology::na.omit(x@data)
  cand_names_plot <- as.character(ei_rc_combine_a[-nrow(ei_rc_combine_a), 1])
  # Subset Point Estimates Just to Differences
  ei_rc_combine_a <- ei_rc_combine_a[
    -nrow(ei_rc_combine_a),
    grep("EI_Diff",
      colnames(ei_rc_combine_a),
      fixed = T
    )
  ] # Gets rid of Total row and selects only DIFF columns
  ei_rc_combine_a <- data.frame(cand_names_plot, ei_rc_combine_a)
  # Reshape the data for ggplot()
  tidy_it <- ei_rc_combine_a %>% tidyr::gather(Group, value, -cand_names_plot)
  # Calculate Standard error differences
  ses <- x@data[x@data$Candidate == "se", ]

  # Conditionals for number of Groups
  if (group_num == 1) { # Group 1 Standard error collect; these all assume only EI/RxC methods
    se_dif_1 <- ses[, 2] - ses[, 3]
    dif_ses <- cbind(cand_names_plot, abs(data.frame(se_dif_1)))
  } else if (group_num == 2) { # Group 2 SE collect
    se_dif_1 <- ses[, 2] - ses[, 3]
    se_dif_2 <- ses[, 5] - ses[, 6]
    dif_ses <- cbind(cand_names_plot, abs(data.frame(se_dif_1, se_dif_2)))
  } else if (group_num == 3) { # Group 3 SE collect
    se_dif_1 <- ses[, 2] - ses[, 3]
    se_dif_2 <- ses[, 5] - ses[, 6]
    se_dif_3 <- ses[, 8] - ses[, 9]
    dif_ses <- cbind(cand_names_plot, abs(data.frame(se_dif_1, se_dif_2, se_dif_3)))
  } else { # Group 4 SE collect
    se_dif_1 <- ses[, 2] - ses[, 3]
    se_dif_2 <- ses[, 5] - ses[, 6]
    se_dif_3 <- ses[, 8] - ses[, 9]
    se_dif_4 <- ses[, 11] - ses[, 12]
    dif_ses <- cbind(cand_names_plot, abs(data.frame(se_dif_1, se_dif_2, se_dif_3, se_dif_4)))
  }
  # Reshape SEs
  tidy_se <- dif_ses %>% tidyr::gather(Group, se, -cand_names_plot)
  # Combine Point esimates with SEs
  tidy_it <- cbind(tidy_it, tidy_se)
  tidy_it$se_1_2 <- tidy_it$se * 2 # Add on
  tidy_it <- tidy_it[, -4] # Get rid of second cand_names_plot
  # Create some NULL values for R CMD CHECK workaround
  value <- Group <- se <- se_1_2 <- NULL
  # Produce the GGPLOT
  suppressWarnings( # Warning: Ignoring unknown aesthetics: y
    ggplot2::ggplot(tidy_it, ggplot2::aes(
      x = cand_names_plot, y = value,
      shape = factor(Group, labels = x@groups),
      color = factor(Group, labels = x@groups)
    )) +
      # Manipulate Point spacing
      ggplot2::geom_point(position = ggplot2::position_dodge(width = rep(.5, nplots)), size = 3) +
      ggplot2::labs(color = "Group", shape = "Group") +
      ggplot2::geom_hline(yintercept = 0, colour = "gray", linetype = 2, size = 1.5) +
      # Adjust Error Bars for 1 and 2 SEs
      ggplot2::geom_linerange(ggplot2::aes(
        x = cand_names_plot, y = value,
        ymax = value + se, ymin = value - se
      ),
      position = ggplot2::position_dodge(width = rep(.5, nplots)), size = 1.5
      ) +
      ggplot2::geom_linerange(ggplot2::aes(
        x = cand_names_plot, y = value,
        ymax = value + se_1_2, ymin = value - se_1_2
      ),
      position = ggplot2::position_dodge(width = rep(.5, nplots)), size = .8
      ) +
      ggplot2::coord_flip() +
      ggplot2::theme_bw() +
      ggplot2::ggtitle("Estimate Difference of EI and RxC Methods") +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 20, face = "bold")) +
      ggplot2::labs(x = "", y = "RxC-EI Estimate")
  ) # Close Warning: Ignoring unknown aesthetics: y message
} # Close Function
