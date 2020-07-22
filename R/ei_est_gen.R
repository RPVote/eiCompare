ei_est_gen <- function(
    cand_vector, 
    race_group, 
    total, 
    rho = 10, 
    data, 
    table_names,
    sample = 1000, 
    tomog = FALSE, 
    density_plot = FALSE, 
    beta_yes = FALSE, 
    verbose = FALSE,
    ...
) {
  list_extract <- function(x) x[, 1:2]
  seq_split <- 2:length(cand_vector)
  if (length(cand_vector) == 1) {
    rn <- c(cand_vector, "se")
  }
  else {
    rn <- c(R.utils::insert(cand_vector, ats = seq_split, values = rep("se", length(cand_vector) - 1)), "se")
  }
  data <- na.omit(data)
  race_group_table <- list()
  beta_full_hold <- list()
  if(!verbose) {
    pb <- txtProgressBar(min = 0, max = length(cand_vector)*length(race_group), style = 3)
    j <- 0
  }
  for (k in 1:length(race_group)) {
    cand_table <- list()
    beta_container <- list()
    for (i in 1:length(cand_vector)) {
      form <- formula(paste(cand_vector[i], race_group[k]))
      try(
        if(!verbose) {
          capture.output({ 
            suppressMessages({
              ei_out <- ei::ei(
                form, total = total, erho = rho, 
                data = data, sample = sample, ...
              )
            })
          })
        } else {
          ei_out <- ei::ei(
            form, total = total, erho = rho,
              data = data, sample = sample, ...
            )
        }, 
      silent = T)
      gm <- geterrmessage()
      if (gm == "Maximizing likelihood\\n         Error in .subset2(x, i, exact = exact) : invalid subscript type 'list'") {
        stop("Maximizing likelihood\\n             Error in .subset2(x, i, exact = exact) : invalid subscript type 'list'\\n\\n             \\n ei package error try re-running ei_est_gen()")
      }
      if(verbose) { 
        cat(paste("Model:", cand_vector[i], race_group[k],
          "\\n",
          sep = " "
        ))
        print(summary(ei_out))
      }
      if (tomog) {
        pdf(paste(cand_vector[i], race_group[k], ".pdf",
          sep = ""
        ))
        plot(ei_out, "tomogE")
        mtext(paste(cand_vector[i], race_group[k], sep = " "),
          outer = T, line = -1
        )
        dev.off()
      }
      if (density_plot) {
        pdf(paste("density_plot", k, i, ".pdf", sep = "_"))
        plot(ei_out, "betab", "betaw")
        mtext(paste(cand_vector[i], race_group[k], sep = " "),
          outer = T, line = -1
        )
        dev.off()
      }

      beta_stan_err <- eiread(
        ei_out, "betab", "sbetab",
        "betaw", "sbetaw"
      )
      min_b <- summary(ei_out)$`Estimates of Aggregate Quantities of Interest`[1, 1] * 100
      min_ste <- summary(ei_out)$`Estimates of Aggregate Quantities of Interest`[1, 2] * 100
      non_b <- summary(ei_out)$`Estimates of Aggregate Quantities of Interest`[2, 1] * 100
      non_ste <- summary(ei_out)$`Estimates of Aggregate Quantities of Interest`[2, 2] * 100

      #  Weighted Mean #
      eimean <- data.frame(c(min_b, min_ste), c(
        non_b,
        non_ste
      ))
      cand_betas <- cbind(unlist(beta_stan_err[1]), unlist(beta_stan_err[3]))
      colnames(cand_betas) <- c("betab", "betaw")
      cand_table[[i]] <- eimean
      beta_container[[i]] <- cand_betas
    
      # increment progressbar
      if(!verbose) {
        j <- j + 1
        setTxtProgressBar(pb, j)
      }
      
    }
    cand_table <- data.table::rbindlist(cand_table)
    cand_table <- data.frame(rn, cand_table)
    race_group_table[[k]] <- cand_table
    beta_full_hold[[k]] <- beta_container
  }
  if (length(race_group) == 1) {
    race_group_table <- data.frame(race_group_table)
    beta_full_hold <- data.frame(beta_full_hold)
    colnames(beta_full_hold) <- c("betab", "betaw")
  }
  else {
    race_group_table <- data.frame(lapply(
      race_group_table,
      list_extract
    ))
    race_group_table <- race_group_table[, c(1, seq(
      2, ncol(race_group_table),
      2
    ))]
  }
  tot <- colSums(race_group_table[seq(
    1, nrow(race_group_table),
    2
  ), 2:ncol(race_group_table)])
  just_data <- race_group_table[, 2:ncol(race_group_table)]
  add <- rbind(just_data, tot)
  add <- data.frame(1:nrow(add), add)
  colnames(add) <- c("Candidate", table_names)
  add[, 1] <- c(as.character(race_group_table[, 1]), "Total")
  race_group_table <- add
  if (beta_yes) {
    beta_names <- list()
    for (i in 1:length(race_group)) {
      beta_names[[i]] <- paste(str_trim(gsub("~", "", race_group[i])),
        cand_vector,
        sep = "_"
      )
    }
    beta_names <- as.vector(unlist(beta_names))
    beta_b <- paste("betab", beta_names, sep = "_")
    beta_w <- paste("betaw", beta_names, sep = "_")
    beta_names <- R.utils::insert(beta_b, ats = 1:length(beta_b) +
      1, values = beta_w)
    beta_full_hold <- as.data.frame(beta_full_hold)
    names(beta_full_hold) <- beta_names
    return(list(race_group_table = race_group_table, all_betas = beta_full_hold))
  }
  else {
    return(race_group_table)
  }
}
