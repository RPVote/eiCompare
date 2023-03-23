#' od_plot_create
#'
#' Internal
#'
#' @param race Racial demographic of interest
#' @param cand_pair All possible candidate pairing combinations
#' @param dens_data Beta values long for each race and candidate pair
#' @param out Summary table from overlay_density_plot for every race candidate
#'  pair
#' @param plot_path Path to save plots. If NULL, plot is not saved.
#' @param cand_colors Colors for every candidate
#' @return Comparison density plots
#' @author Loren Collingwood <loren.collingwood@@ucr.edu>
#' @author Hikari Murayama
#'
#' @return overlay density plot comparing candidates for votes by race
#'
#' @importFrom overlapping overlap
#' @importFrom stats setNames
#' @import ggplot2
#'
#' @export


od_plot_create <- function(race,
                           cand_pair,
                           dens_data,
                           out,
                           plot_path = NULL,
                           cand_colors) {
  # Set new variables to NULL
  value <- Candidate <- sd_minus <- sd_plus <- NULL

  # Omit NAs and subset both density data and summmary table
  dens_data_sub <- na.omit(
    dens_data[dens_data$Candidate %in% gsub("pct_", "", cand_pair), ]
  )
  out_sub <- out[out$Candidate %in% gsub("pct_", "", cand_pair), ]
  # Calculate overlap
  overlap_list <- list(
    X1 = dens_data_sub[
      dens_data_sub$Candidate == gsub("pct_", "", cand_pair[1]), "value"
    ],
    X2 = dens_data_sub[
      dens_data_sub$Candidate == gsub("pct_", "", cand_pair[2]), "value"
    ]
  )
  overlap_out <- overlapping::overlap(overlap_list, plot = FALSE)
  # Extract overlap percentage
  overlap_perc <- overlap_out$OV[[1]] * 100
  # Extract overlap point
  overlap_point <- overlap_out$xpoints$`X1-X2`[[1]]

  # colors
  cols <- stats::setNames(
    c(
      cand_colors[gsub("pct_", "", cand_pair[1])][[1]],
      cand_colors[gsub("pct_", "", cand_pair[2])][[1]]
    ),
    c(gsub("pct_", "", cand_pair[1]), gsub("pct_", "", cand_pair[2]))
  )

  # factor
  dens_data_sub$Candidate <- factor(dens_data_sub$Candidate,
    levels = gsub("pct_", "", cand_pair),
    ordered = TRUE
  )


  densplot <- ggplot2::ggplot(
    dens_data_sub,
    ggplot2::aes(x = value, fill = Candidate)
  ) +
    # Set colors according to candidate
    scale_fill_manual(values = cols, aesthetics = c("color", "fill")) +
    # Add titles
    ggplot2::ggtitle(paste0(
      gsub("pct_", "", cand_pair[1]), " vs ",
      gsub("pct_", "", cand_pair[2]), " for ",
      gsub("pct_", "", race), " voters (overlap: ",
      round(overlap_perc, 2), "%)"
    )) +
    ggplot2::xlab("Percent of vote") +
    ggplot2::ylab("Density") +
    ggplot2::geom_density(
      alpha = 0.5,
      ggplot2::aes_string(
        x = "value * 100",
        y = "..scaled..",
        colour = "Candidate"
      ),
      adjust = 2
    ) +
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
    ggplot2::geom_text(
      x = max(out_sub$sd_plus[1], out_sub$sd_minus[1]),
      y = .12,
      label = "sigma",
      size = 3,
      parse = TRUE
    ) +
    ggplot2::geom_text(
      x = min(out_sub$sd_minus[2], out_sub$sd_plus[2]),
      y = .08,
      label = "sigma",
      size = 3,
      parse = TRUE
    ) +
    # Add text label for means
    ggplot2::geom_label(
      x = out_sub$mean_size[1],
      y = 1.09, label = paste("mu==", round(out_sub$mean_size[1], 2)),
      size = 2, show.legend = NA, fill = "white", parse = TRUE
    ) +
    ggplot2::geom_label(
      x = out_sub$mean_size[2],
      y = 1.03, label = paste("mu==", round(out_sub$mean_size[2], 2)),
      size = 2, show.legend = NA, fill = "white", parse = TRUE
    ) +
    # Set limits for plot
    ggplot2::xlim(0, 100) +
    ggplot2::scale_y_continuous(
      breaks = c(0, 0.25, 0.5, 0.75, 1.0),
      limits = c(0, 1.1)
    ) +
    # Formatting
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      axis.title.x = ggplot2::element_text(face = "bold"),
      axis.title.y = ggplot2::element_text(face = "bold")
    )


  # Save out to user designated path
  if (!is.null(plot_path)) {
    ggplot2::ggsave(paste0(
      plot_path, cand_pair[1], "_", cand_pair[2], "_",
      gsub("pct_", "", race), ".png"
    ), height = 4, width = 6)
  }
  

  return(densplot)
}
