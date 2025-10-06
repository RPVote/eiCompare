rpv_coef_plot <- function (rpvDF = NULL, 
                            title = "Racially Polarized Voting Analysis Estimates", 
                            caption = "Data: eiCompare RPV estimates", 
                            ylab = "Pct. Voting Black-Preferred Candidate", 
                            colors = NULL, 
                            race_order = c("Black", "White")
) 
  
{
  colnames(rpvDF) <- stringr::str_to_lower(colnames(rpvDF))
  
  rpvDF$voter_race <- factor(rpvDF$voter_race, levels = race_order)
  len_race <- length(unique(rpvDF$voter_race))
  if (is.null(colors)) {
    if (len_race == 2) {
      race_colors <- c(viridis::viridis(10)[4], viridis::viridis(10)[7])
      names(race_colors) <- race_order
      ggplot_color_obj <- scale_color_manual(values = race_colors)
    }
    else {
      ggplot_color_obj <- viridis::scale_color_viridis(drop = FALSE, 
                                                       discrete = TRUE, option = "turbo", alpha = 0.8)
    }
  }
  if (is.null(ylab)) {
    prty <- unique(rpvDF$party) %>% stringr::str_to_title()
    ylab <- paste("Percent Voting for", prty, "Candidate")
  }
  
  coef_plot <- ggplot(rpvDF, aes(x = 0, y = 0:100)) + 
    scale_y_continuous(breaks = seq(0, 100, by = 10), 
                       limits = c(0, 100), 
                       labels = sprintf("%0.1f%%", seq(0, 100, by = 10)), 
                       expand = c(0, 0)) + geom_hline(yintercept = 50, 
                                                      colour = "#000000", size = 0.75) + 
    geom_pointrange(aes(y = .data$estimate, 
                        ymin = .data$lower_bound, 
                        ymax = .data$upper_bound, 
                        color = .data$voter_race),
                    position = position_jitter(width = 0.1), 
                    linewidth = .5, fatten = 1.5, 
                    show.legend = F) + 
    ggplot_color_obj + 
    facet_grid(~panellab) + 
    labs(y = ylab, title = title, caption = caption) + theme_minimal() + 
    theme(legend.title = element_blank(), axis.title.x = element_blank(), 
          axis.ticks.x = element_blank(), axis.text.x = element_blank(), 
          panel.border = element_rect(fill = NA, colour = "grey"), 
          panel.grid.major.x = element_blank(), 
          panel.grid.minor.x = element_blank(), 
          panel.grid.minor.y = element_blank(), 
          axis.text.y = element_text(size = 20, face = "bold", 
                                     family = "serif"), 
          axis.title.y = element_text(size = 24, 
                                      face = "bold", family = "serif"), 
          strip.text.x = element_text(size = 15, 
                                      face = "bold", family = "serif"), 
          title = element_text(size = 25, hjust = 0.5, face = "bold", family = "serif"), 
          plot.caption = element_text(size = 12, face = "italic", 
                                      family = "serif"))
  return(coef_plot)
}
