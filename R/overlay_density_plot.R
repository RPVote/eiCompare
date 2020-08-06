#' overlay_density_plot
#'
#' Internal
#'
#' @param betas Output for RxC and iterative ei
#' @param path Path to save
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
utils::globalVariables(c("m"))

overlay_density_plot <- function(betas, path, ei_type) {
  if (tolower(ei_type) == "ei") {
    race <- unique(stringr::str_match(names(betas), "beta[bw]_([a-z_]*)_pct")[, 2])
    cands <- unique(stringr::str_match(names(betas), "beta[bw]_[a-z_]*_(pct_[a-z]*)")[, 2])
    # Extract beta bs
    betas <- betas[, grep("betab", colnames(betas))]
  } else if (tolower(ei_type) == "rxc") {
    race <- unique(stringr::str_match(names(betas), ".*\\.([a-z_]*)\\..*")[, 2])
    cands <- unique(stringr::str_match(names(betas), ".*\\.[a-z_]*\\.(.*)")[, 2])
  } else {
    stop("Specify ei_type as ei or rxc")
  }

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
    i = seq(1:length(race)),
    .combine = rbind,
    .inorder = FALSE,
    .packages = c("overlapping", "ggplot2"),
    .options.snow = opts
  ) %do% {

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
        min_size = min(value * 100, na.rm = TRUE),
        max_size = max(value * 100, na.rm = TRUE), .groups = "drop"
      )
    out <- data.frame(out)

    # Create pairs of candidates to comapre
    cand_comb <- utils::combn(levels(dens_data$Candidate), 2)

    # Make density plot for each pair
    dens_plots <- foreach::foreach(m = seq(1:ncol(cand_comb)), .inorder = FALSE, .verbose = TRUE) %do% {
      print(c(cand_comb[1, m][[1]], cand_comb[2, m][[1]]))
      od_plot_create(
        race = race[i], cand_comb = c(cand_comb[1, m][[1]], cand_comb[2, m][[1]]),
        dens_data, out, path, cand_colors
      )
    }

    setTxtProgressBar(pb, i)
  }

  return(dens_plots)
}
