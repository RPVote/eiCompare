plot.ei_compare <- function(x, ...){
  
  # Calculate Number of Plots to Create
  group_num <- nplots <- length(x@groups) # Up to 4 groups!
  # Warnings
  if(group_num > 4) stop("Number of groups must be four or fewer booyah!") # Stop plot() if more than four racial/ethnic groups (for plotting purposes)
  if(!is.na(x@data[2,4])) stop("In 'ei_rc_good_table()' function set include_good=F, I'm cereal you guys")
  
  # Collect Candidate Names for Labeling
  ei_rc_combine_a <- na.omit(x@data)
  cand_names_plot <- as.character(ei_rc_combine_a[-nrow(ei_rc_combine_a),1])
  # Subset Point Estimates Just to Differences
  ei_rc_combine_a <- ei_rc_combine_a[-nrow(ei_rc_combine_a), grep("EI_Diff",colnames(ei_rc_combine_a), fixed=T) ] # Gets rid of Total row and selects only DIFF columns
  ei_rc_combine_a <- data.frame(cand_names_plot,ei_rc_combine_a )
  # Reshape the data for ggplot()
  tidy_it <- ei_rc_combine_a %>% tidyr::gather(Group, value, -cand_names_plot)
  # Calculate Standard error differences
  ses <- x@data[x@data$Candidate=="se",]
  
  # Conditionals for number of Groups 
  if (group_num ==1) { # Group 1 Standard error collect; these all assume only EI/RxC methods
    se_dif_1 <- ses[,2] - ses[,3]
    dif_ses <- cbind(cand_names_plot, abs( data.frame(se_dif_1) ))
  } else if (group_num ==2) { # Group 2 SE collect
    se_dif_1 <- ses[,2] - ses[,3]
    se_dif_2 <- ses[,5] - ses[,6]
    dif_ses <- cbind(cand_names_plot, abs( data.frame(se_dif_1, se_dif_2) ))
  } else if (group_num ==3) { # Group 3 SE collect
    se_dif_1 <- ses[,2] - ses[,3]
    se_dif_2 <- ses[,5] - ses[,6]
    se_dif_3 <- ses[,8] - ses[,9]
    dif_ses <- cbind(cand_names_plot, abs ( data.frame(se_dif_1, se_dif_2, se_dif_3) ))
  } else { # Group 4 SE collect
    se_dif_1 <- ses[,2] - ses[,3]
    se_dif_2 <- ses[,5] - ses[,6]
    se_dif_3 <- ses[,8] - ses[,9]
    se_dif_4 <- ses[,11] - ses[,12]
    dif_ses <- cbind(cand_names_plot, abs( data.frame(se_dif_1, se_dif_2, se_dif_3, se_dif_4) ))
  } 
  # Reshape SEs
  tidy_se <- dif_ses %>% tidyr::gather(Group, se, -cand_names_plot)
  # Combine Point esimates with SEs
  tidy_it <- cbind(tidy_it, tidy_se)
  tidy_it$se_1_2 <-tidy_it$se*2 # Add on 
  tidy_it <- tidy_it[,-4] # Get rid of second cand_names_plot
  # Create some NULL values for R CMD CHECK workaround
  value <- Group <- se <- se_1_2 <- NULL
  # Produce the GGPLOT
  suppressWarnings( # Warning: Ignoring unknown aesthetics: y
  ggplot(tidy_it,aes(x=cand_names_plot, y = value, 
                     shape=factor(Group, labels=x@groups), 
                     color=factor(Group, labels=x@groups))) +
    # Manipulate Point spacing
    geom_point(position=position_dodge(width=rep(.5,nplots)),size=3) + labs(color = "Group", shape="Group") +
    geom_hline(yintercept = 0, colour="gray", linetype = 2, size=1.5) +
    # Adjust Error Bars for 1 and 2 SEs
    geom_linerange(aes(x=cand_names_plot, y=value,  
                       ymax = value + se, ymin = value - se), 
                   position=position_dodge(width=rep(.5,nplots)), size=1.5) +
    geom_linerange(aes(x=cand_names_plot, y=value, ymax = value + se_1_2, ymin = value - se_1_2),
                   position=position_dodge(width=rep(.5,nplots)), size=.8) +
    coord_flip() +
    theme_bw()  +
    ggtitle("Estimate Difference of EI and RxC Methods")+
    theme(plot.title = element_text(size = 20, face = "bold")) +
    labs(x="", y="RxC-EI Estimate") 
  ) # Close Warning: Ignoring unknown aesthetics: y message
} # Close Function

