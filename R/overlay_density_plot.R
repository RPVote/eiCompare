#' overlay_density_plot
#'
#' Internal
#'
#' @param betas Output for RxC and iterative ei
#' @param results_table Summary table for candidate race pair means and se's
#' @param plot_path Path to save
#' @param ei_type Specify whether the data comes from iterative ei ("ei") or rxc ("rxc")
#' @return Prep and run density plot creation iteratively
#' @author Loren Collingwood <loren.collingwood@@ucr.edu>
#' @author Hikari Murayama
#'
#' @examples
#'
#'
#' # EXAMPLE: NOT RUN #
#' @export
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
    k = 1:length(race),
    .combine = rbind,
    .inorder = FALSE,
    .packages = c("overlapping", "ggplot2"),
    .options.snow = opts
  ) %do% {

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
        min_size = min(value * 100, na.rm = TRUE),
        max_size = max(value * 100, na.rm = TRUE), .groups = "drop"
      )
    out <- data.frame(out)
    out$mean_size <- results_table[!(results_table$Candidate %in% c("se", "Total")), race[k]]
    out$sd_minus <- out$mean_size - out$sd_size
    out$sd_plus <- out$mean_size + out$sd_size

    # Create pairs of candidates to comapre
    cand_comb <- utils::combn(cands, 2)

    # Make density plot for each pair
<<<<<<< HEAD
    dens_plots <- foreach::foreach(m = 1:ncol(cand_comb)) %do% {
      dens_plot <- od_plot_create(
        race = race[k], cand_comb = c(cand_comb[1, m][[1]], cand_comb[2, m][[1]]),
        dens_data, out, plot_path = plot_path, cand_colors
=======
    dens_plots <- foreach::foreach(m = seq(1:ncol(cand_comb)), .inorder = FALSE, .verbose = TRUE) %do% {
      print(c(cand_comb[1, m][[1]], cand_comb[2, m][[1]]))
      od_plot_create(
        race = race[i], cand_comb = c(cand_comb[1, m][[1]], cand_comb[2, m][[1]]),
        dens_data, out, path, cand_colors
>>>>>>> Improving and cleaning plotting functions
      )
    }

    setTxtProgressBar(pb, k)
  }
  # close progress bar
  close(pb)


  return(dens_plots)
}
