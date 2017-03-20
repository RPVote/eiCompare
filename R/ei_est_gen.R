ei_est_gen <- function(cand_vector, race_group, total, rho=10, data,
                       table_names, sample=1000, tomog=F, density_plot=F,
                       beta_yes=F,...) {
  
  # Functions
  list_extract <- function(x) x[,1:2] # sends to lapply to extract indiv column estimates
  
  # Table/Output Row Labeling -- #Added this if statement in to handle calls to one candidate
  seq_split <- 2:length(cand_vector)
  if (length(cand_vector) == 1) {
    rn <- c(cand_vector, "se")
  } else {
    rn <- c(insert(cand_vector, ats= seq_split,values=rep("se",length(cand_vector)-1)), "se")
  }
  # Remove any missing datas
  data <- na.omit(data) 
  
  #Loop Placeholder
  race_group_table <- list()
  beta_full_hold <- list() # Added; 2-25-17
  
  # Loop over Race Vector
  for (k in 1:length(race_group)) {
    
    # Loop Placeholder
    cand_table <- list() # candidate place holder
    beta_container <- list() # Added; 2-25-17
    
    # Loop over Candidates
    for (i in 1:length(cand_vector)) {
      
      # Formula object that is looked through
      form <- formula(paste(cand_vector[i], race_group[k])) 
      try(ei_out <- ei::ei(form, total = total, erho=rho, data=data, sample=sample,...),silent=T)
      gm <- geterrmessage()
      if(gm == "Maximizing likelihood
         Error in .subset2(x, i, exact = exact) : invalid subscript type 'list'") 
        stop("Maximizing likelihood
             Error in .subset2(x, i, exact = exact) : invalid subscript type 'list'\n
             \n ei package error try re-running ei_est_gen()"
        )
      
      cat(paste("Model:",cand_vector[i], race_group[k], "\n",sep=" "))
      
      print(summary(ei_out))
      
      #Tomography plot
      if (tomog) {
        pdf(paste(cand_vector[i], race_group[k], ".pdf",sep=""))
        plot(ei_out, "tomogE")
        mtext(paste(cand_vector[i], race_group[k], sep=" "), outer=T, line=-1)
        dev.off()
      }
      # Print Out Out density plot for examination
      if(density_plot) {
        pdf(paste("density_plot",k,i,".pdf",sep="_"))
        plot(ei_out, "betab","betaw") # Plot out distribution plots
        mtext(paste(cand_vector[i], race_group[k], sep=" "), outer=T, line=-1)
        dev.off()
      } 
      
      # Extract Beta B and W
      beta_stan_err <- eiread(ei_out, "betab", "sbetab", "betaw", "sbetaw") # beta estimate for minority group
      min_b <- mean(unlist(beta_stan_err[1]), na.rm=T)*100
      min_ste <- mean(unlist(beta_stan_err[2]), na.rm=T)*100
      non_b <- mean(unlist(beta_stan_err[3]), na.rm=T)*100
      non_ste <- mean(unlist(beta_stan_err[4]), na.rm=T)*100
      # Put into useable data frame		
      eimean <- data.frame(c(min_b, min_ste), c(non_b, non_ste)) 
      
      cand_betas <- cbind( unlist(beta_stan_err[1]), unlist(beta_stan_err[3]) )
      colnames(cand_betas) <- c("betab", "betaw")
      #the results for all candidate are stored here in this list
      cand_table[[i]] <- eimean
      beta_container[[i]] <- cand_betas # Betas
      
    } # Close cand_vector loop
    
    cand_table <- data.table::rbindlist(cand_table) # cand_table is for one racial group and all candidates
    cand_table <- data.frame(rn, cand_table) # Add in vector for labeling
    
    race_group_table[[k]] <- cand_table # Put candidate results into list
    beta_full_hold[[k]] <- beta_container # add in all the betas
    
  } # Close race group loop
  
  if(length(race_group) == 1) { # For when there is just % Minority vs. % White, for example
    
    race_group_table <- data.frame(race_group_table)
    beta_full_hold <- data.frame(beta_full_hold) # convert to data.frame()
    colnames(beta_full_hold) <- c("betab", "betaw")
    
  } else{ # For when there are multiple groups (e.g., pct_hisp, pct_asian, pct_white)
    
    race_group_table <- data.frame( lapply(race_group_table, list_extract) ) # list is length() number of racial groups
    race_group_table <- race_group_table[,c(1,seq(2,ncol(race_group_table),2))] # clean up table
  
  }
  # Adding on Total Row
  tot <- colSums(race_group_table[seq(1,nrow(race_group_table),2),2:ncol(race_group_table)])
  just_data <- race_group_table[,2:ncol(race_group_table)]
  add <- rbind(just_data, tot)
  add <- data.frame(1:nrow(add), add)
  colnames(add) <- c("Candidate", table_names)
  add[,1] <- c(as.character(race_group_table[,1]), "Total")
  race_group_table <- add 
  
  # Conditional Export of Betas; Default is only Table
  if (beta_yes){ # probably need to put in conditional here for race_group > 1
    beta_names <-list()
    for (i in 1:length(race_group)){
      beta_names[[i]] <- paste(str_trim(gsub("~","",race_group[i])), cand_vector, sep="_") 
    }
    # Unlist the pasting, then organize for col-names labeling
    beta_names <- as.vector( unlist(beta_names) )
    beta_b <- paste("betab", beta_names, sep="_")
    beta_w <- paste("betaw", beta_names, sep="_")
    beta_names <- insert(beta_b, ats=1:length(beta_b)+1, values=beta_w) # this insert function makes no sense
    
    beta_full_hold <- as.data.frame(beta_full_hold)
    names(beta_full_hold) <- beta_names
    
    # Return results, and then also betas
    return(list(race_group_table=race_group_table, all_betas = beta_full_hold))
  
  } else {
    return(race_group_table)
  }
} # Close Function