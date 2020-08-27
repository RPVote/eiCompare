#' Print a plot comparing the predictions of EI outputs.
#'
#' The output of this plot consists of error bars containing the mean for each
#' candidate, racial group, and eiCompare object. Error bars represent one
#' standard deviation from the mean of the posterior sampling distribution.
#'
#' @param x An eiCompare object, outputted from ei_iter() or ei_rxc().
#' @param ... Additional eiCompare objects to summarize.
#' @return A ggplot comparing eiCompare objects.
#'
#' @import ggplot2
#' @importFrom rlang .data
#'
#' @export
plot.eiCompare <- function(x, ...) {
  # Consolidate eiCompare objects
  objects <- list(x, ...)
  n_objects <- length(objects)

  # Consolidate EI outputs in a single dataframe
  data <- as.data.frame(matrix(nrow = 0, ncol = 7))
  for (ii in seq_along(objects)) {
    object <- objects[[ii]]
    samples <- as.data.frame(object$estimates)
    # handle blank names
    if (object$name == "") {
      samples["name"] <- paste0("result_", ii)
    } else {
      samples["name"] <- object$name
    }
    data <- rbind(data, samples)
  }

  # Get unique values for race and candidates
  races <- unique(data$race)
  cands <- unique(data$cand)
  data$race <- factor(data$race, levels = races)
  data$cand <- factor(data$cand, levels = rev(cands))

  # Get errorbars
  data$lower <- data$mean - data$sd
  data$upper <- data$mean + data$sd
  data$lower[data$lower < 0] <- 0
  data$upper[data$upper > 1] <- 1
  
  # Construct error bar plot
  ggplot2::ggplot(
    data = data,
    ggplot2::aes(x = .data$mean, y = .data$cand, fill = .data$name)
  ) +
    ggplot2::geom_errorbarh(
      ggplot2::aes(
        xmin = .data$lower,
        xmax = .data$upper
      ),
      height = 0.25,
      position = ggplot2::position_dodge(width = n_objects * 0.25)
    ) +
    ggplot2::geom_point(
      size = 2.5,
      shape = 21,
      position = ggplot2::position_dodge(width = n_objects * 0.25)
    ) +
    ggplot2::geom_vline(
      xintercept = 0.5,
      linetype = "dashed",
      color = "red"
    ) +
    ggplot2::facet_grid(.data$race ~ .) +
    ggplot2::scale_fill_brewer(type = "qual") +
    ggplot2::scale_x_continuous(
      name = "Proportion of vote",
      breaks = seq(0, 1, .2),
      limits = c(0, 1)
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "bottom",
      axis.title.y = ggplot2::element_blank(),
      legend.title = ggplot2::element_blank()
    )
}
