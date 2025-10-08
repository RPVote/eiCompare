#' @export
#' @import ggplot2
#' @importFrom rlang .data
#'
#' @author Rachel Carroll <rachelcarroll4@gmail.com>
#' @author Stephen El-Khatib <stevekhatib@gmail.com>
#' @author Loren Collingwood <lcollingwood@unm.edu>
#'
#' @title Racially Polarized Voting Analysis (RPV) Coefficient Plot
#' @description Creates a coefficient plot showing of RPV results estimate ranges
#' of all contests by voter race
#' @param rpvDF A data.frame containing RPV results
#' @param title The plot title
#' @param caption The plot caption
#' @param ylab Label along y axis
#' @param colors Character vector of colors, one for each racial group. The order
#' of colors will be respective to the order of racial groups.
#' @param race_order Character vector of racial groups from the \code{voter_race} column of
#' \code{rpvDF} in the order they should appear in the plot. If not specified,
#' the race groups will appear in alphabetical order.
#'
#' @return Coefficient plot of RPV analysis as a ggplot2 object
#'
#' @examples
#'library(eiCompare)
#'data(example_rpvDF)
#'
#'dem_rpv_results <- example_rpvDF %>% dplyr::filter(Party == "Democratic")
#'rpv_coef_plot(dem_rpv_results)
#'
rpv_coef_plot <- function(
    rpvDF = NULL,
    title = "Racially Polarized Voting Analysis Estimates",
    caption = "Data: eiCompare RPV estimates",
    ylab = NULL,
    colors = NULL,
    race_order = NULL
  ) {

  # -----------------------------    QC CHECKS    -----------------------------

  colnames(rpvDF) <- stringr::str_to_lower(colnames(rpvDF))

  ##### new code (copied from eiExpand lines 40-58)
  # make sure rpvDF argument is defined
  if(is.null(rpvDF)){stop("you must include rpvDF argument")}

  # make sure necessary columns are included
  dif <- setdiff(c("party", "voter_race", "estimate", "lower_bound", "upper_bound"),
          colnames(rpvDF))

  if( length(dif) > 0 ) {
    stop(paste("rpvDF is missing the following fields:",
               paste(dif, collapse = ", ")))
  }

  # make sure only one party is in rpvDF
  if( length(unique(rpvDF$party)) > 1 ){
    stop("rpvDF should only contain one unique values in column Party")}
  ##### end QC checks

  # ----------------------   Prep Data and Plot Inputs   ----------------------

  ##### Voter Race Order #####
  ##### old code (from Updates_7_1_2024.R)
  # rpvDF$voter_race <- factor(rpvDF$voter_race, levels = race_order)
  ##### new code (copied from eiExpand lines 64-69)
  # proper case for plot
  rpvDF$voter_race <- stringr::str_to_title(rpvDF$voter_race)
  #get factor order if not specified
  if( is.null(race_order) ) { race_order <- sort(unique(rpvDF$voter_race)) }
  #set factor
  rpvDF$voter_race <- factor(rpvDF$voter_race,
                             levels = race_order)

  ##### Colors #####
  len_race <- length(unique(rpvDF$voter_race))
  ##### old code (from Updates_7_1_2024.R)
  # if (is.null(colors)) {
  #   if (len_race == 2) {
  #     race_colors <- c(viridis::viridis(10)[4], viridis::viridis(10)[7])
  #     names(race_colors) <- race_order
  #     ggplot_color_obj <- scale_color_manual(values = race_colors)
  #   }
  #   else {
  #     ggplot_color_obj <- viridis::scale_color_viridis(drop = FALSE,
  #                                                      discrete = TRUE, option = "turbo", alpha = 0.8)
  #   }
  # }
  ##### new code (copied from eiExpand lines 71-85)
  if( is.null(colors) ){
    if( len_race == 2 ){
      race_colors <- c(viridis::viridis(10)[4], viridis::viridis(10)[7])
      names(race_colors) <- race_order

      ggplot_color_obj <- scale_color_manual(values = race_colors)

    } else {
      ggplot_color_obj <- viridis::scale_color_viridis(drop = FALSE,
                                                       discrete = TRUE,
                                                       option = "turbo",
                                                       alpha = .8)
    }
  } # END  if( is.null(colors) )

  ##### ylab #####
  if( is.null(ylab) ){
    prty <- unique(rpvDF$party) %>% stringr::str_to_title()
    ylab <- paste("Percent Voting for", prty,  "Candidate")
  }

  ##### mean percent vote for label #####
  mean <- rpvDF %>%
    dplyr::group_by(.data$voter_race) %>%
    dplyr::summarize(avg = mean(.data$estimate))

  rpvDF <- dplyr::left_join(rpvDF, mean, by = "voter_race")
  rpvDF$panelLab <- paste0(rpvDF$voter_race, "\n(mean: ", round(rpvDF$avg,1), "%)")

  # --------------------------      Build Plot       --------------------------

  coef_plot <- ggplot(rpvDF,
                      aes(x = 0, y = 0:100)) +
    scale_y_continuous(breaks = seq(0,100, by = 10),
                       limits = c(0, 100),
                       labels = sprintf("%0.1f%%", seq(0,100, by = 10)),
                       expand = c(0, 0)) +
    geom_hline(yintercept = 50, colour = "#000000", size = 0.75) +  # Line at 0
    geom_pointrange(aes(y = .data$estimate,
                        ymin = .data$lower_bound,
                        ymax = .data$upper_bound,
                        color = .data$voter_race),
                    position = position_jitter(width = 0.1),
                    size = 2,
                    fatten = 1.5,
                    show.legend = F) +  # Ranges for each coefficient
    ggplot_color_obj +
    facet_grid(~panelLab) +
        labs(y = ylab,
         title = title,
         caption = caption) +  # Labels
    theme_minimal() +
    theme(legend.title = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x  = element_blank(),
          panel.border = element_rect(fill = NA, colour = "grey"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.text.y = element_text(size = 20, face = "bold", family = "serif"),
          axis.title.y = element_text(size = 24, face = "bold", family = "serif"),
          strip.text.x = element_text(size = 15, face = "bold", family = "serif"),
          #strip.text.x = element_blank(),
          title = element_text(size = 30, hjust = .5, face = "bold", family = "serif"),
          plot.caption = element_text(size = 12, face = "italic", family = "serif")
          )

# --------------------------        Return         --------------------------
  return(coef_plot)
}
