#' rpv_density
#'
#'
#' @param agg_betas Aggregated beta values
#' @param plot_path Path to save
#' @return Return density for every race/candidate pair for Bb-Bw
#' @author Loren Collingwood <loren.collingwood@@ucr.edu>
#' @author Hikari Murayama
#'
#' @examples
#'
#'
#' # EXAMPLE: NOT RUN #
#' @export
#'
#'
#'

# pair wise subtraction
rpv_density <- function(agg_betas, plot_path) {
  value <- NULL


  # take difference between pairs
  agg_betas_diff <- matrix(ncol = ncol(agg_betas) / 2, nrow = nrow(agg_betas))
  for (i in seq(1, ncol(agg_betas), 2)) {
    agg_betas_diff[, (i + 1) / 2] <- agg_betas[, i] - agg_betas[, i + 1]
  }
  colnames(agg_betas_diff) <- unique(gsub("b[bw]gg_", "", colnames(agg_betas)))

  # melt long needed columns
  agg_betas_diff <- data.frame(reshape2::melt(agg_betas_diff))

  agg_betas_diff$race <- stringr::str_match(agg_betas_diff$Var2, "pct_([a-z_]*)_pct")[, 2]
  agg_betas_diff$cand <- stringr::str_match(agg_betas_diff$Var2, "[a-z_]*_pct_([a-z_]*)")[, 2]

  # Plot as facet
  rpv_plot <- ggplot(data = agg_betas_diff) +
    ggplot2::geom_density(alpha = 0.25, ggplot2::aes_string(x = "value * 100", y = "..scaled.."), adjust = 1) +
    ggplot2::facet_grid(cand ~ race) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      axis.title.x = ggplot2::element_text(face = "bold"),
      axis.title.y = ggplot2::element_text(face = "bold")
    ) +
    # Vertical line at 0
    ggplot2::geom_vline(xintercept = 0, color = "red", linetype = "dotted") +
    ggplot2::xlab("Bb - Bw") +
    ggplot2::ylab("Density")

  ggplot2::ggsave(paste0(
    plot_path, "rpv_density.png"
  ), height = 4, width = 6)

  return(rpv_plot)
}
