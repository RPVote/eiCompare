#' overlay_density_plot
#'
#' Internal
#'
#' @param betas Output for RxC and iterative ei
<<<<<<< HEAD
#' @param results_table Summary table for candidate race pair means and se's
#' @param plot_path Path to save
#' @param ei_type Specify whether the data comes from iterative ei ("ei") or rxc ("rxc")
=======
#' @param path Path to save
#' @param ei_type
>>>>>>> integrate overlays and pass R CMD
#' @return Prep and run density plot creation iteratively
#' @author Loren Collingwood <loren.collingwood@@ucr.edu>
#' @author Hikari Murayama
#'
#' @examples
#'
#'
#' # EXAMPLE: NOT RUN #
#' @export
<<<<<<< HEAD
utils::globalVariables(c("m", "k", "%do%"))

overlay_density_plot <- function(betas, results_table, plot_path, ei_type) {
  if (tolower(ei_type) == "ei") {
    race <- unique(stringr::str_match(colnames(betas), "b[bw]gg_([a-z_]*)_pct")[, 2])
    cands <- unique(stringr::str_match(colnames(betas), "b[bw]gg_[a-z_]*_(pct_[a-z]*)")[, 2])
    # Extract beta bs
    betas <- betas[, grep("bbgg", colnames(betas))]
  } else if (tolower(ei_type) == "rxc") {
    race <- unique(stringr::str_match(colnames(betas), ".*\\.([a-z_]*)\\..*")[, 2])
    cands <- unique(stringr::str_match(colnames(betas), ".*\\.[a-z_]*\\.(.*)")[, 2])
  } else {
    stop("Specify ei_type as ei or rxc")
=======
utils::globalVariables(c("m"))

overlay_density_plot <- function(betas, path, ei_type) {
  if (ei_type == "ei") {
    race <- unique(stringr::str_match(names(betas), "beta[bw]_([a-z_]*)_pct")[, 2])
    cands <- unique(stringr::str_match(names(betas), "beta[bw]_[a-z_]*_(pct_[a-z]*)")[, 2])
    # Extract beta bs
    betas <- betas[, grep("betab", colnames(betas))]
  } else if (ei_type == "rxc") {
    race <- unique(stringr::str_match(names(betas), ".*\\.([a-z_]*)\\..*")[, 2])
    cands <- unique(stringr::str_match(names(betas), ".*\\.[a-z_]*\\.(.*)")[, 2])
  }
>>>>>>> integrate overlays and pass R CMD

  # Designate colors for each candidate
  color_choice <- as.list(RColorBrewer::brewer.pal(length(unique(cands)), "Set2"))
  cand_colors <- stats::setNames(color_choice, as.list(gsub("pct_", "", cands)))

  # go through each
  # Iterate through every race...
  pb <- utils::txtProgressBar(
    min = 0,
    max = length(race),
    style = 3
  )
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)

  out_all <- foreach::foreach(
<<<<<<< HEAD
    k = 1:length(race),
=======
    i = seq(1:length(race)),
>>>>>>> integrate overlays and pass R CMD
    .combine = rbind,
    .inorder = FALSE,
    .packages = c("overlapping", "ggplot2"),
    .options.snow = opts
  ) %do% {

<<<<<<< HEAD
    # Keep columns for race[k]
    race_comb <- betas[, grep(race[k], colnames(betas))]

    # Column titles
    colnames(race_comb) <- gsub(paste("bbgg_", race[k], ".", sep = ""), "", colnames(race_comb))

    # Set up data to create graphs
    colnames(race_comb) <- gsub("pct_", "", colnames(race_comb))
    dens_data <- reshape2::melt(race_comb, )


    colnames(dens_data) <- c("Candidate", "value")

    out <- dens_data %>%
      dplyr::group_by(Candidate) %>%
      dplyr::summarize(
        sd_size = sd(value * 100, na.rm = TRUE),
=======
    # Keep columns for race[i]
    race_comb <- betas[, grep(race[i], colnames(betas))]

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
      dplyr::group_by(Candidate) %>%
      summarize(
        mean_size = mean(value * 100, na.rm = TRUE),
        sd_size = sd(value * 100, na.rm = TRUE),
        sd_minus = mean(value * 100, na.rm = TRUE) - sd(value * 100, na.rm = TRUE),
        sd_plus = mean(value * 100, na.rm = TRUE) + sd(value * 100, na.rm = TRUE),
>>>>>>> integrate overlays and pass R CMD
        min_size = min(value * 100, na.rm = TRUE),
        max_size = max(value * 100, na.rm = TRUE), .groups = "drop"
      )
    out <- data.frame(out)
<<<<<<< HEAD
    out$mean_size <- results_table[!(results_table$Candidate %in% c("se", "Total")), race[k]]
    out$sd_minus <- out$mean_size - out$sd_size
    out$sd_plus <- out$mean_size + out$sd_size

    # Create pairs of candidates to comapre
    cand_comb <- utils::combn(cands, 2)

    # Make density plot for each pair
    dens_plots <- foreach::foreach(m = 1:ncol(cand_comb)) %do% {
      dens_plot <- od_plot_create(
        race = race[k], cand_comb = c(cand_comb[1, m][[1]], cand_comb[2, m][[1]]),
        dens_data, out, plot_path = plot_path, cand_colors
      )
    }

    setTxtProgressBar(pb, k)
  }
  # close progress bar
  close(pb)

  # Change functionalist to return plots later if needed
=======

    # Create pairs of candidates to comapre
    cand_comb <- utils::combn(levels(dens_data$Candidate), 2)

    # Make density plot for each pair
    dens_plots <- foreach::foreach(m = seq(1:ncol(cand_comb)), .inorder = FALSE) %do% {
      od_plot_create(m, i, race, cand_comb, dens_data, out, path, cand_colors)
    }

    setTxtProgressBar(pb, i)
  }

>>>>>>> integrate overlays and pass R CMD
  return(dens_plots)
}
