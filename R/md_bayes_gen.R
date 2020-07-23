md_bayes_gen <- function(
  data, 
  cand_cols,
  race_cols,
  totals_col = NULL, 
  ntunes = 10, 
  totaldraws = 10000,
  seed = 12345, 
  sample = 1000, 
  thin = 100, 
  burnin = 10000,
  ret.mcmc = TRUE, 
  ci = c(0.025, 0.975), 
  ci_TRUE = TRUE, 
  produce_draws = FALSE,
  verbose = FALSE,
  ...
) {
  # get formula
  formula <- as.formula(
    paste(
      'cbind(',
      paste(cand_cols, collapse = ", "),
      ') ~ cbind(',
      paste(race_cols, collapse = ", "),
      ")",
      sep = ""
    )
  )
  
  set.seed(seed)
  if (!is.null(totals_col)) { # When variables are percents #

    # Tune it real good #
    if (verbose == TRUE) { cat("\nTune the tuneMD real good...\n") }

    suppressWarnings(tune.nocov <- tuneMD(formula,
      data = data, ntunes = ntunes,
      totaldraws = totaldraws, total = totals_col, ...
    ))

    # Estimate Bayes Model -- can take a while (real good)

    if (verbose == TRUE) {
      cat("\nHello my name is Simon and I like to do ei.MD.bayes drawrings...\n")
    }
      
    suppressWarnings(md.out <- ei.MD.bayes(formula,
      data = data, sample = sample, total = totals_col,
      thin = thin, burnin = burnin, ret.mcmc = ret.mcmc,
      tune.list = tune.nocov, ...
    ))
  } else { # When variables are raw numeros #

    # Tune it so good #
    if (verbose == TRUE) { cat("\nTune the tuneMD real good...\n") }

    tune.nocov <- tuneMD(formula,
      data = data, ntunes = ntunes,
      totaldraws = totaldraws, ...
    )

    # Estimate Bayes Model real good
    if (verbose == TRUE) {
      cat("\nAnd you know my name is Simon and I like to do ei.MD.bayes() drawrings...\n")
    }

    md.out <- ei.MD.bayes(formula,
      data = data, sample = sample,
      thin = thin, burnin = burnin, ret.mcmc = ret.mcmc,
      tune.list = tune.nocov, ...
    )
  }

  # Extract MD Bayes Cells #
  md_draw <- md.out$draws$Cell.counts

  # Clean up formula
  name_extract_rxc <- function(form_object, num) {
    form <- gsub("cbind(", "", as.character(formula)[num], fixed = T)
    form <- gsub(")", "", form, fixed = T)
    var <- unlist(strsplit(form, ","))
    str_squish(var)
  }

  # Race & Candidates #
  race <- name_extract_rxc(formula, 3)
  candidates <- name_extract_rxc(formula, 2)

  race_list <- list()

  # For loop along Race #
  for (i in 1:length(race)) { # open up i loop

    # Pull MD-Draws #
    race_comb <- md_draw[, grep(race[i], colnames(md_draw))]

    total <- apply(race_comb, 1, sum)
    v_fill <- matrix(NA, nrow = nrow(race_comb), ncol = ncol(race_comb))

    for (j in 1:ncol(v_fill)) { # open j loop

      v_fill[, j] <- race_comb[, j] / total
    } # close j loop

    if (ci_TRUE) { # Get Confidence Intervals #

      qtile <- cbind(
        mcse.mat(v_fill) * 100, mcse.q.mat(v_fill, q = ci[1])[, 1] * 100,
        mcse.q.mat(v_fill, q = ci[2])[, 1] * 100
      )

      colnames(qtile) <- c("Mean", "SE", paste(ci[1] * 100, collapse = ""), paste(ci[2] * 100, collapse = ""))
      rownames(qtile) <- candidates
    } else { # Don't get Confidence Intervals #

      qtile <- cbind(mcse.mat(v_fill) * 100)

      colnames(qtile) <- c("Mean", "SE")
      rownames(qtile) <- candidates
    }

    race_list[[i]] <- qtile
  } # close initial i loop #

  names(race_list) <- race

  # Produce both the table and the draws for draw analysis #
  # Two-item list #

  if (produce_draws) {
    return(list(table = race_list, draws = md_draw))
  } else { # Just the table #

    return(race_list)
  }
} # Close Function
