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
#' @export
#'
#' @return ggplot object with bivariate plots faceted by candidate and race
plot_bivariate <- function(data, cand_cols, race_cols) {
  data <- data[, c(cand_cols, race_cols)]

  n_races <- length(race_cols)
  n_cands <- length(cand_cols)
  dot_size <- min(4 / log(n_races * n_cands), 3)

  # Lengthen data over candidates
  cand_long <- as.data.frame(matrix(ncol = n_races + 2, nrow = 0))
  for (i in 1:n_cands) {
    cand <- cand_cols[i]

    # Drop other candidate columns
    other_cands <- cand_cols[which(cand_cols != cand)]
    temp <- data[, -which(names(data) %in% other_cands)]

    # Get correct colname
    colnames(temp)[which(colnames(temp) == cand)] <- "pct_of_vote"
    temp$candidate <- cand

    # Add temp to dataframe
    cand_long <- rbind(cand_long, temp)
  }

  # Lengthen data over races
  data_long <- as.data.frame(matrix(ncol = 4, nrow = 0))
  for (i in 1:n_races) {
    race <- race_cols[i]

    # Drop other race columns
    other_races <- race_cols[which(race_cols != race)]
    temp <- cand_long[, -which(names(cand_long) %in% other_races)]

    # Get correct colname
    colnames(temp)[which(colnames(temp) == race)] <- "pct_of_voters"
    temp$race <- race

    # Add temp to dataframe
    data_long <- rbind(data_long, temp)
  }


  # data_for_plot <- data %>%
  #  tidyr::pivot_longer(
  #    cand_cols,
  #    names_to = "candidate",
  #    values_to = "pct_of_vote"
  #  ) %>%
  #  tidyr::pivot_longer(
  #    race_cols,
  #    names_to = "race",
  #    values_to = "pct_of_voters"
  #  )

  ggplot2::ggplot(
    data = data_long,
    aes(x = pct_of_voters, y = pct_of_vote)
  ) +
    ggplot2::geom_point(alpha = 0.5, size = dot_size) +
    ggplot2::facet_grid(candidate ~ race) +
    ggplot2::scale_x_continuous(
      limits = c(0, 1),
      name = "Race % of Total Turnout",
      expand = c(0.02, 0.02)
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 1),
      name = "Candidate % of Total Votes",
      expand = c(0.02, 0.02)
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.minor = element_blank(),
      axis.title = element_text(face = "bold"),
      panel.spacing = unit(0.5, "lines")
    )
}
