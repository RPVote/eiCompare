#' overlay_density_plot
#'
#' Internal
#'
#' @param agg_betas Output for RxC and iterative ei
#' @param results_table Summary table for candidate race pair means and se's
#' @param race_cols A character vector listing the column names for turnout by race
#' @param cand_cols A character vector listing the column names for turnout for each candidate
#' @param plot_path Path to save
#' @param ei_type Specify whether the data comes from iterative ei ("ei") or rxc ("rxc")
#' @return Prep and run density plot creation iteratively
#' @author Loren Collingwood <loren.collingwood@@ucr.edu>
#' @author Hikari Murayama
#'
#' @importFrom foreach %dopar% %do%
#'
#' @export
#'


overlay_density_plot <- function(agg_betas, results_table, race_cols, cand_cols, plot_path, ei_type) {
  # Set new variables to NULL
  k <- Candidate <- value <- m <- NULL


  race <- race_cols
  cands <- cand_cols

  if (tolower(ei_type) == "ei") {
    # Extract beta bs
    agg_betas <- agg_betas[, grep("bbgg", colnames(agg_betas))]
    colnames(agg_betas) <- gsub("bbgg_", "", colnames(agg_betas))
  } else if (tolower(ei_type) == "rxc") {
    colnames(agg_betas) <- gsub("betas.", "", colnames(agg_betas))
    colnames(agg_betas) <- gsub("\\.", "_", colnames(agg_betas))
  } else {
    stop("Specify ei_type as ei or rxc")
  }


  # Designate colors for each candidate
  # Designate color blind friendly scale
  # from Color University Design at University of Tokyo
  color_scale <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")
  # Repeat twice so can accomodate up to 16 candidates (with repeating colors)
  color_scale <- c(color_scale, color_scale)
  color_choice <- as.list(color_scale[1:length(unique(cands))])
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
    .inorder = TRUE,
    .packages = c("overlapping", "ggplot2"),
    .options.snow = opts
  ) %do% {

    # Keep columns for race[k]
    race_comb <- agg_betas[, grep(race[k], colnames(agg_betas))]

    # Column titles
    colnames(race_comb) <- gsub(paste(race[k], "_", sep = ""), "", colnames(race_comb))

    # Set up data to create graphs
    dens_data <- reshape2::melt(race_comb, id.vars = NULL)

    if (ncol(dens_data) == 3) {
      dens_data <- dens_data[, c(2:3)]
    }
    colnames(dens_data) <- c("Candidate", "value")

    out <- dens_data %>%
      dplyr::group_by(Candidate) %>%
      dplyr::summarize(
        sd_size = sd(value * 100, na.rm = TRUE),
        min_size = min(value * 100, na.rm = TRUE),
        max_size = max(value * 100, na.rm = TRUE),
        .groups = "drop",
      )
    out <- data.frame(out)

    # Join with mean from results_table
    if (ei_type == "ei") {
      rt_sub <- results_table[!(results_table$Candidate %in% c("se", "Total")), c("Candidate", race[k])]
      colnames(rt_sub) <- c("Candidate", "mean_size")
    } else if (ei_type == "rxc") {
      dens_data <- as.data.frame(dens_data[, c("Candidate", "value")])

      rt_sub <- as.data.frame(results_table[race[k]][[1]])
      rt_sub$Candidate <- rownames(rt_sub)
      rt_sub <- dplyr::rename(rt_sub, mean_size = mean)
      rt_sub <- rt_sub[, c("Candidate", "mean_size")]
    }


    out <- inner_join(rt_sub, out, by = "Candidate")
    out$sd_minus <- out$mean_size - out$sd_size
    out$sd_plus <- out$mean_size + out$sd_size

    dens_data$Candidate <- gsub("pct_", "", dens_data$Candidate)
    out$Candidate <- gsub("pct_", "", out$Candidate)

    # Create pairs of candidates to comapre
    cand_comb <- utils::combn(cands, 2)

    # Make density plot for each pair
    dens_plots <- foreach::foreach(m = 1:ncol(cand_comb)) %do% {
      dens_plot <- od_plot_create(
        race = race[k], cand_pair = c(cand_comb[1, m], cand_comb[2, m]),
        dens_data, out, plot_path = plot_path, cand_colors
      )
    }

    setTxtProgressBar(pb, k)
  }
  # close progress bar
  close(pb)

  # Change functionalist to return plots later if needed
  return(out_all)
}
