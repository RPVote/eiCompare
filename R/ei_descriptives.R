#' Plot bivariate relationships between all combinations of candidates and
#' race/ethnicities
#'
#' @import ggplot2
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#'
#' @param data A data.frame() object containing precinct-level turnout data by
#' race and candidate
#' @param cand_cols A character vector listing the column names for turnout for
#' each candidate
#' @param race_cols A character vector listing the column names for turnout by
#' race
#' @param corrs A boolean indicating whether to include correlation coefficients
#' on the plot.
#' @param save A boolean indicating whether to save the plot to a file.
#' @param path A string to specify plot save location. If NULL, plot is not saved.
#'
#' @export
#'
#' @return ggplot object with bivariate plots faceted by candidate and race
plot_bivariate <- function(
                           data,
                           cand_cols,
                           race_cols,
                           corrs = FALSE,
                           save = FALSE,
                           path = NULL) {
  data <- data[, c(cand_cols, race_cols)]

  n_races <- length(race_cols)
  n_cands <- length(cand_cols)
  dot_size <- min(4 / log(n_races * n_cands), 3)

  # Get data to long format for faceting
  data_long <- data %>%
    tidyr::pivot_longer(
      cand_cols,
      names_to = "candidate",
      values_to = "pct_of_vote"
    ) %>%
    tidyr::pivot_longer(
      race_cols,
      names_to = "race",
      values_to = "pct_of_voters"
    )

  # Change candidate, race cols to factor so their order matches
  # cand_cols, race_cols order
  data_long$candidate <- factor(data_long$candidate,
    levels = cand_cols
  )

  data_long$race <- factor(data_long$race,
    levels = race_cols
  )

  bivariate_plot <- ggplot2::ggplot(
    data = data_long,
    ggplot2::aes(x = .data$pct_of_voters, y = .data$pct_of_vote)
  ) +
    ggplot2::geom_smooth(method='lm', color = "red") + 
    ggplot2::geom_point(alpha = 0.3, size = dot_size) + 
   # ggplot2::geom_point(alpha = 0.5, size = dot_size) +
    ggplot2::facet_grid(.data$candidate ~ .data$race) +
    ggplot2::scale_x_continuous(
      limits = c(0, 1),
      name = "Race % of Turnout",
      expand = c(0.02, 0.02)
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 1),
      name = "Candidate % of Votes",
      expand = c(0.02, 0.02)
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(face = "bold"),
      panel.spacing = ggplot2::unit(1.25, "lines")
    )

  # Add correlations if requested.
  if (corrs) {
    corrs <- race_cand_cors(data, cand_cols, race_cols)
    corrs$candidate <- rownames(corrs)
    corrs_long <- corrs %>%
      tidyr::pivot_longer(race_cols, names_to = "race", values_to = "corr")
    bivariate_plot <- bivariate_plot +
      ggplot2::geom_text(
        data = corrs_long,
        ggplot2::aes(label = paste("r = ", round(.data$corr, 2), sep = "")),
        x = .91,
        y = 0.93,
        size = ifelse(max(n_races, n_cands) > 3, 2, 3)
      )
  }

  if (save & !is.null(path)) {
    ggplot2::ggsave(
      paste0(path, "bivariate_plot_", n_cands, "x", n_races, ".png"),
      bivariate_plot,
      height = 2 * n_cands,
      width = 2 * n_races
    )
  }
  return(bivariate_plot)
}

#' Table of bivariate correlations
#'
#' @param data A data.frame() object containing precinct-level turnout data by
#' race and candidate
#' @param cand_cols A character vector listing the column names for turnout for
#' each candidate
#' @param race_cols A character vector listing the column names for turnout by
#' race
#' @return a dataframe of correlation coefficients describing to correlation between the racial proportion of a precinct and the vote share of each candidate.
#' @export
#'
#' @importFrom stats cor
race_cand_cors <- function(data, cand_cols, race_cols) {
  as.data.frame(cor(data[, c(cand_cols)], data[, c(race_cols)]))
}
