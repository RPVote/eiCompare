#' Plot eiCompare objects for comparison
#'
#' @param eic_objects A named list of eiCompare objects to be compared in plot.
#' Names will be used to label each object in the plot legend.
#'
#' @export
#'
#' @return A ggplot2 object
#'

plot_eic <- function(eic_objects) {
  data <- as.data.frame(matrix(nrow = 0, ncol = 7))
  for (i in 1:length(eic_objects)) {
    object <- eic_objects[[i]]
    samples <- as.data.frame(object$estimates)
    samples["estimate"] <- names(eic_objects)[i]
    data <- rbind(data, samples)
  }

  races <- unique(data$race)
  cands <- unique(data$cand)
  data$race <- factor(data$race, levels = races)
  data$cand <- factor(data$cand, levels = rev(cands))

  ggplot(data = data, aes(x = mean, y = cand, fill = estimate)) +
    geom_errorbarh(
      aes(
        xmin = ci_95_lower,
        xmax = ci_95_upper
      ),
      height = 0.25
    ) +
    geom_point(size = 2.5, shape = 21) +
    geom_vline(xintercept = 0.5, linetype = "dashed", color = "red") +
    facet_grid(race ~ .) +
    scale_fill_brewer(type = "qual") +
    scale_x_continuous(
      name = "Proportion of vote",
      breaks = seq(0, 1, .2)
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      axis.title.y = element_blank(),
      legend.title = element_blank()
    )
}
