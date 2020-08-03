#' od_plot_create
#'
#' Internal
#'
#' @param m
#' @param i
#' @param cand_comb
#' @param dens_data
#' @param out
#' @param path
#' @param cand_colors
#' @return Comparison density plots
#' @author Loren Collingwood <loren.collingwood@@ucr.edu>
#' @author Hikari Murayama
#'
#' @return overlay density plot comparing candidates for votes by race
#' @examples
#'
#'
#' # EXAMPLE: NOT RUN #
#' @export

utils::globalVariables(c("fips_col_temp", "value", "Candidate", "..scaled", "sd_minus", "sd_plus"))


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
    ggplot2::geom_text(x = out_sub$sd_minus[1], y = .12, label = expression(sigma), size = 2) +
    ggplot2::geom_text(x = out_sub$sd_minus[2], y = .08, label = expression(sigma), size = 2) +
    # Add text label for means
    ggplot2::geom_label(
      x = out_sub$mean_size[1],
      y = 1.09, label = paste(expression(mu), "=", round(out_sub$mean_size[1], 2), sep = " "),
      size = 2, show.legend = NA, fill = "white"
    ) +
    ggplot2::geom_label(
      x = out_sub$mean_size[2],
      y = 1.03, label = paste(expression(mu), "=", round(out_sub$mean_size[2], 2), sep = " "),
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

  return(densplot)
}
