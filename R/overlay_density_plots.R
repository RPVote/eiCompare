###############
# load packages
###############
library(usethis)
library(devtools)
library(foreign)
library(dplyr)
library(grid)

library(ggplot2)
library(reshape2)
library(stringr)
# library(mcmcse)

devtools::install_github("DSSG-eiCompare/eiCompare@master")
library(eiCompare)

install.packages("overlapping")
library(overlapping)

install.packages("pbapply")
library(pbapply)

install.packages(c("foreach", "doParallel", "dslabs"))
suppressPackageStartupMessages({
  library(foreach)
  library(parallel)
  library(doParallel)
  library(dslabs)
  library(datasets)
})

###############
# load sample data
###############

# load csv data of jacksonville
setwd("/srv/shared/week_1_2")
list.files()

# Read in
jx <- read.csv("jacksonville_fl_2010_senate.csv")

# check to make sure that eiCompare is 2.4 version
packageVersion("eiCompare")

# creating pct_cands variables
# IN EACH PRECINCT, THE SUMMATION OF ALL CANDIDATE SUPPORT NEEDS TO BE 1
# WHICH IS WHY I CREATE THE OTHERCANDS VARIABLE
jx$pct_rubio <- jx$rubio / jx$tot_vote
jx$pct_meek <- jx$meek / jx$tot_vote
jx$pct_crist <- jx$crist / jx$tot_vote
jx$pct_othercands <- 1 - jx$pct_crist - jx$pct_meek - jx$pct_rubio

# creating pct_race variables
# IN EACH PRECINCT, THE SUMMATION OF ALL RACIAL GROUPS NEEDS TO BE 1
# WHICH IS WHY I CREATE THE OTHERFOLKS VARIABLE
jx$pct_black <- jx$black_voters / jx$tot_vote
jx$pct_white <- jx$white_voters / jx$tot_vote
jx$pct_latino <- jx$latino_voters / jx$tot_vote
jx$pct_otherfolks <- 1 - jx$pct_white - jx$pct_black - jx$pct_latino

# filter out negative values
# R CAN SOMETIMES ROUND NUMBERS IN A WEIRD WAY, THIS JUST REMOVES NEGATIVE VALUES
jx <- filter(.data = jx, pct_othercands >= 0 & pct_otherfolks >= 0)

# remove duplicate precincts
jx <- filter(.data = jx, duplicated(jx$precinct) == FALSE)

# check for NAs
jx <- na.omit(jx)
summary(jx$pct_otherfolks)
summary(jx$pct_othercands)

summary(jx)

###############
# ei
###############

# create our paramaters for ei and RxC
cands <- c("pct_rubio", "pct_meek", "pct_crist", "pct_othercands")
race_group <- c("~ pct_black", "~ pct_white", "~ pct_latino", "~ pct_otherfolks")
table_names <- c("EI: Pct Black", "EI: Pct White", "EI: Pct Latino", "EI: Pct Other")
formula <- formula(cbind(pct_rubio, pct_meek, pct_crist, pct_othercands) ~ cbind(pct_black, pct_white, pct_latino, pct_otherfolks))

# set seed
set.seed(1997)

# run King's iterative EI
results_ei <- ei_est_gen(
  cand_vector = cands, race_group = race_group,
  total = "tot_vote", data = jx, table_names = table_names,
  beta_yes = TRUE
)
results_rxc <- md_bayes_gen(dat = jx, form = formula, total = "tot_vote", produce_draws = TRUE)
betas_ei <- results_ei [[2]] # 1 would give the summary estimates
betas_rxc <- data.frame(results_rxc$draws)


# save path for plots
path <- "~/shared/density_voter_choice/rxc/"
path2 <- "~/shared/density_voter_choice/ei/"

###############
# function testing
###############

###############
# plotting function

od_plot_create <- function(m, i, race, cand_comb, dens_data, out, path, cand_colors) {
  # Store data for candidates
  myCands <- c(cand_comb[1, m][[1]], cand_comb[2, m][[1]])
  dens_data_sub <- na.omit(dens_data[dens_data$Candidate %in% myCands, ])
  out_sub <- out[out$Candidate %in% myCands, ]

  # Calculate overlap
  overlap_list <- list(
    X1 = dens_data_sub[dens_data_sub$Candidate == cand_comb[1, m], "value"],
    X2 = dens_data_sub[dens_data_sub$Candidate == cand_comb[2, m], "value"]
  )
  overlap_out <- overlapping::overlap(overlap_list, plot = FALSE)
  # Extract overlap percentage
  overlap_perc <- overlap_out$OV[[1]] * 100
  # Extract overlap point
  overlap_point <- overlap_out$xpoints$`X1-X2`[[1]]

  # colors
  cols <- c(cand_colors[myCands[1]], cand_colors[myCands[2]])
  names(cols) <- c(myCands[1], myCands[2])

  densplot <- ggplot2::ggplot(dens_data_sub, ggplot2::aes(x = value, fill = Candidate)) +
    # Set colors according to candidate
    scale_fill_manual(values = cols) +
    # Add titles
    ggplot2::ggtitle(paste0(
      cand_comb[1, m], " vs ",
      cand_comb[2, m], " for ",
      gsub("pct_", "", race[i]), " voters (overlap: ",
      round(overlap_perc, 2), "%)"
    )) +
    ggplot2::xlab("Percent of vote") +
    ggplot2::ylab("Density") +
    ggplot2::geom_density(alpha = 0.25, ggplot2::aes(x = value * 100, y = ..scaled..), adjust = 2) +
    # Add vertical line for halfway
    ggplot2::geom_vline(xintercept = 50, color = "black", linetype = "dotted") +
    # Add vertical lines for means for density
    ggplot2::geom_vline(
      xintercept = out_sub$mean_size[1],
      color = "red",
      linetype = "dotted"
    ) +
    ggplot2::geom_vline(
      xintercept = out_sub$mean_size[2],
      color = "blue",
      linetype = "dotted"
    ) +
    # Add horizontal lines for standard deviation
    ggplot2::geom_segment(ggplot2::aes(
      x = sd_minus[1],
      y = .1,
      xend = sd_plus[1],
      yend = .1
    ),
    color = "red",
    linetype = "dashed", data = out_sub
    ) +
    ggplot2::geom_segment(aes(
      x = sd_minus[2],
      y = .1,
      xend = sd_plus[2],
      yend = .1
    ),
    color = "blue",
    linetype = "dashed", data = out_sub
    ) +
    # Add sigma label
    ggplot2::geom_text(x = out_sub$sd_minus[1], y = .12, label = "std", size = 2) +
    ggplot2::geom_text(x = out_sub$sd_minus[2], y = .08, label = "std", size = 2) +
    # Add text label for means
    ggplot2::geom_label(
      x = out_sub$mean_size[1],
      y = 1.09, label = paste("µ =", round(out_sub$mean_size[1], 2), sep = " "),
      size = 2, show.legend = NA, fill = "white"
    ) +
    ggplot2::geom_label(
      x = out_sub$mean_size[2],
      y = 1.03, label = paste("µ =", round(out_sub$mean_size[2], 2), sep = " "),
      size = 2, show.legend = NA, fill = "white"
    ) +
    # Set limits for plot
    ggplot2::xlim(0, 100) +
    ggplot2::scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1.0), limits = c(0, 1.1)) +
    # Formatting
    ggplot2::theme_bw()


  # Save out to user designated path
  ggplot2::ggsave(paste0(
    path, myCands[[1]], "_", myCands[[2]], "_",
    gsub("pct_", "", race[i]), ".png"
  ), height = 4, width = 6)
}

###############
# density plot for rxc
#
# overlay_density_plot_rxc <- function(md_draw, path) {
#   # get race vector
#   race <- unique(str_match(names(md_draw), ".*\\.([a-z_]*)\\..*")[, 2])
#
#   # go through each
#   # Iterate through every race...
#
#   out_all <- foreach( i = seq(1:length(race)),
#                       .combine = rbind,
#                       .inorder = FALSE,
#                       .packages = c("overlapping", "ggplot2","data.table","dplyr")) %:%
#
#     # Keep columns for race[i]
#     race_comb <- md_draw[, grep(race[i], colnames(md_draw))]
#
#     # Find total for race[i] in each row (so for each precinct)
#     total <- apply(race_comb, 1, sum)
#
#     # Set up empty matrix
#     v_fill <- matrix(NA, nrow = nrow(race_comb), ncol = ncol(race_comb))
#     # For each candidate find percentage that voted for that candidate
#     for (j in 1:ncol(v_fill)) {
#       v_fill[, j] <- race_comb[, j] / total
#     }
#
#     # Column titles
#     colnames(v_fill) <- gsub(paste("ccount.", race[i], ".", sep = ""), "", colnames(race_comb))
#
#     # Set up data to create graphs
#     colnames(v_fill) <- gsub("pct_", "", colnames(v_fill))
#     dens_data <- melt(v_fill)
#     colnames(dens_data) <- c("Index", "Candidate", "value")
#
#     out <- dens_data %>%
#       group_by(Candidate) %>%
#       summarize(
#         mean_size = mean(value * 100, na.rm = TRUE),
#         sd_size = sd(value * 100, na.rm = TRUE),
#         sd_minus = mean(value * 100, na.rm = TRUE) - sd(value * 100, na.rm = TRUE),
#         sd_plus = mean(value * 100, na.rm = TRUE) + sd(value * 100, na.rm = TRUE),
#         min_size = min(value * 100, na.rm = TRUE),
#         max_size = max(value * 100, na.rm = TRUE), .groups = "drop"
#       )
#     out <- data.frame(out)
#     out$race <- race[i]
#
#     # Create pairs of candidates to comapre
#     cand_comb <- combn(levels(dens_data$Candidate), 2)
#
#     # Make density plot for each pair
#     pb <- txtProgressBar(min = 0, max = length(cand_comb), style = 3)
#     foreach(m = seq(1:ncol(cand_comb)), .inorder = FALSE) %do% {
#     # for (m in seq(1:ncol(cand_comb))) {
#       # Set up a progress bar
#       setTxtProgressBar(pb, m)
#       # Create plots
#       od_plot_create(m, cand_comb, dens_data, out, path)
#     }
#
#     out
#
#
#   # # ALWAYS STOP CLUSTER BETWEEN USES
#   # stopCluster(clust)
#   #
#   # # Garbage collection (in case of leakage)
#   # gc()
#   #
#   # return(out_all)
# }
#
# # # Detect the number of cores you have
# # detectCores()
# # # Standard to use 1 less core for clusters
# # clust <- makeCluster(detectCores() - 1)
# #
# # # Register parallel processing cluster
# # registerDoParallel(clust)
# #
# # # Check to make sure that cores are set up correctly
# # getDoParWorkers()
#
# start_time= Sys.time()
# results_test <- overlay_density_plot_rxc(betas_rxc, path)
# (end_time = Sys.time() - start_time)
#
# # ALWAYS STOP CLUSTER BETWEEN USES
# stopCluster(clust)
#
# # Garbage collection (in case of leakage)
# gc()

###############
# density plot for ei
### repetitive to above considering combining with rxc depending on how ei_est_gen turns out

overlay_density_plot_ei <- function(betas_ei, path) {
  race <- unique(str_match(names(betas_ei), "beta[bw]_([a-z_]*)_pct")[, 2])
  cands <- unique(str_match(names(betas_ei), "beta[bw]_[a-z_]*_(pct_[a-z]*)")[, 2])
  # Designate colors for each candidate
  color_choice <- as.list(rainbow(length(unique(dens_data$Candidate))))
  cand_colors <- setNames(color_choice, as.list(gsub("pct_", "", cands)))
  # Extract beta bs
  b_betas_ei <- betas_ei[, grep("betab", colnames(betas_ei))]

  # go through each
  # Iterate through every race...
  out_all <- foreach(i = seq(1:length(race)), .combine = rbind, .inorder = FALSE, .packages = c("overlapping", "ggplot2", "tcltk")) %do% {

    # Keep columns for race[i]
    race_comb <- b_betas_ei[, grep(race[i], colnames(b_betas_ei))]

    # Find total for race[i] in each row (so for each precinct)
    total <- apply(race_comb, 1, sum)

    # Set up empty matrix
    v_fill <- matrix(NA, nrow = nrow(race_comb), ncol = ncol(race_comb))
    # For each candidate find percentage that voted for that candidate
    for (j in 1:ncol(v_fill)) {
      v_fill[, j] <- race_comb[, j] / total
    }

    # Column titles
    colnames(v_fill) <- gsub(paste("betab_", race[i], ".", sep = ""), "", colnames(race_comb))

    # Set up data to create graphs
    colnames(v_fill) <- gsub("pct_", "", colnames(v_fill))
    dens_data <- reshape2::melt(v_fill)
    colnames(dens_data) <- c("Index", "Candidate", "value")

    out <- dens_data %>%
      group_by(Candidate) %>%
      summarize(
        mean_size = mean(value * 100, na.rm = TRUE),
        sd_size = sd(value * 100, na.rm = TRUE),
        sd_minus = mean(value * 100, na.rm = TRUE) - sd(value * 100, na.rm = TRUE),
        sd_plus = mean(value * 100, na.rm = TRUE) + sd(value * 100, na.rm = TRUE),
        min_size = min(value * 100, na.rm = TRUE),
        max_size = max(value * 100, na.rm = TRUE), .groups = "drop"
      )
    out <- data.frame(out)

    # Create pairs of candidates to comapre
    cand_comb <- combn(levels(dens_data$Candidate), 2)

    # Make density plot for each pair
    # pb <- txtProgressBar(min = 0, max = length(cand_comb), style = 3)
    foreach(m = seq(1:ncol(cand_comb)), .inorder = FALSE) %do% {
      # Set up a progress bar
      # setTxtProgressBar(pb, m)
      od_plot_create(m, i, race, cand_comb, dens_data, out, path, cand_colors)
    }

    out$race <- race[i]
    out
  }

  return(out_all)
}

start_time <- Sys.time()
results_test2 <- overlay_density_plot_ei(betas_ei, path2)
(end_time <- Sys.time() - start_time)

###############
# density plot for both with ei rxc toggle

# overlay_density_plot_ei <- function(betas_ei, path, ei_type) {
#   if ei_type == "ei" {
#     race <- unique(str_match(names(betas_ei), "beta[bw]_([a-z_]*)_pct")[, 2])
#     cands <- unique(str_match(names(betas_ei), "beta[bw]_[a-z_]*_(pct_[a-z]*)")[, 2])
#
#     b_betas_ei <- betas_ei[, grep("betab", colnames(betas_ei))]
#   } else if ei_type == "rxc" {
#     race <- unique(str_match(names(md_draw), ".*\\.([a-z_]*)\\..*")[, 2])
#   } else {
#     stop("invalid ei_type. specify ei or rxc")
#   }
#
#   # go through each
#   # Iterate through every race...
#   out_all <- foreach(i = seq(1:length(race)),.combine = rbind,.inorder = FALSE,.packages = c("overlapping", "ggplot2","tcltk")) %do% {
#
#     # Keep columns for race[i]
#     race_comb <- b_betas_ei[, grep(race[i], colnames(b_betas_ei))]
#
#     # Find total for race[i] in each row (so for each precinct)
#     total <- apply(race_comb, 1, sum)
#
#     # Set up empty matrix
#     v_fill <- matrix(NA, nrow = nrow(race_comb), ncol = ncol(race_comb))
#     # For each candidate find percentage that voted for that candidate
#     for (j in 1:ncol(v_fill)) {
#       v_fill[, j] <- race_comb[, j] / total
#     }
#
#     # Column titles
#     colnames(v_fill) <- gsub(paste("betab_", race[i], ".", sep = ""), "", colnames(race_comb))
#
#     # Set up data to create graphs
#     colnames(v_fill) <- gsub("pct_", "", colnames(v_fill))
#     dens_data <- melt(v_fill)
#     colnames(dens_data) <- c("Index", "Candidate", "value")
#
#     out <- dens_data %>%
#       group_by(Candidate) %>%
#       summarize(
#         mean_size = mean(value * 100, na.rm = TRUE),
#         sd_size = sd(value * 100, na.rm = TRUE),
#         sd_minus = mean(value * 100, na.rm = TRUE) - sd(value * 100, na.rm = TRUE),
#         sd_plus = mean(value * 100, na.rm = TRUE) + sd(value * 100, na.rm = TRUE),
#         min_size = min(value * 100, na.rm = TRUE),
#         max_size = max(value * 100, na.rm = TRUE), .groups = "drop"
#       )
#     out <-data.frame(out)
#
#     # Create pairs of candidates to comapre
#     cand_comb <- combn(levels(dens_data$Candidate), 2)
#
#     # Make density plot for each pair
#     # pb <- txtProgressBar(min = 0, max = length(cand_comb), style = 3)
#     foreach(m = seq(1:ncol(cand_comb)), .inorder = FALSE) %do% {
#       # Set up a progress bar
#       # setTxtProgressBar(pb, m)
#       # od_plot_create(m, i, cand_comb, dens_data, out, path)
#       od_plot_create(m, i, race, cand_comb, dens_data, out, path)
#     }
#
#     out$race <- race[i]
#   }
#
#   return(out_all)
# }
#
# start_time = Sys.time()
# results_test2 <- overlay_density_plot_ei(betas_ei, path2)
# (end_time = Sys.time() - start_time)
