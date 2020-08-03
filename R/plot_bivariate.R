#' Plot bivariate relationships
#'
#' @import ggplot2
#'
#' @param data A data.frame() object containing precinct-level turnout data by
#' race and candidate
#' @param cand_cols A character vector listing the column names for turnout for
#' each candidate
#' @param race_cols A character vector listing the column names for turnout by
#' race
#'
#' @return ggplot object with bivariate plots faceted by candidate and race
plot_bivariate <- function(data, cand_cols, race_cols) {
  cols <- length(race_cols)
  rows <- length(cand_cols)
  dot_size <- min(4 / log(cols * rows), 3)
  data %>%
    tidyr::pivot_longer(
      cand_cols,
      names_to = "candidate",
      values_to = "pct_of_vote"
    ) %>%
    tidyr::pivot_longer(
      race_cols,
      names_to = "race",
      values_to = "pct_of_voters"
    ) %>%
    mutate("candidate_by_race" = paste0(candidate, ", ", race)) %>%
    ggplot(aes(x = pct_of_voters, y = pct_of_vote)) +
    geom_point(alpha = 0.5, size = dot_size) +
    facet_grid(candidate ~ race) + # , nrow = rows, ncol = cols) +
    scale_x_continuous(
      limits = c(0, 1),
      name = "Race % of Total Turnout",
      expand = c(0.02, 0.02)
    ) +
    scale_y_continuous(
      limits = c(0, 1),
      name = "Candidate % of Total Votes",
      expand = c(0.02, 0.02)
    ) +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      axis.title = element_text(face = "bold"),
      panel.spacing = unit(0.5, "lines")
    )
}
