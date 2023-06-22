#' Congruence for 2x2
#'
#' Calculates congruence scores between EI and RxC for the 2x2 Scenario
#'
#'
#' @param ei_rc_table Object produced from ei_rc_good_table(), where
#' include_good=F, of class ei_compare
#' @param cand_race Numeric vector indicating race of the candidates in order
#' they show up in table rownames, where 1=Latino; 2=Black; 3=Asian;
#' 4=White/Non
#' @param group_race Numeric vector, taking similar values as cand_race where
#' 1=Latino; 2=Black; 3=Asian; 4=White/Non
#' @return Table of congruence scores
#' @author Loren Collingwood <loren.collingwood@@ucr.edu>, Matt Barreto
#' <barretom@@ucla.edu>
#'
#' @export ei_rc_congruence
ei_rc_congruence <- function(ei_rc_table, cand_race, group_race) {
  dat <- ei_rc_table@data # extract data table
  dat[, 1] <- as.character(dat[, 1]) # convert to character
  # Up to 4 groups
  n_groups <- length(ei_rc_table@groups)
  message(paste0("Number of Groups is: ", n_groups, "\n"))
  if (n_groups == 2) { # Number of groups = 2
    dat <- dat[, c(1:3, 5:6)]
  } else if (n_groups == 3) { # Number of groups = 3
    dat <- dat[, c(1:3, 5:6, 8:9)]
  } else { # Number of groups = 4
    dat <- dat[, c(1:3, 5:6, 8:9, 11:12)]
  }
  # Set dataframe to just candidates
  dat <- dat[dat$Candidate != "se" & dat$Candidate != "Total", ]
  dat$cand_race <- cand_race
  dat <- dat[order(dat$cand_race), ] # Sort so that minority candidate comes first
  message(paste0("Number of Candidates is: ", nrow(dat), "\n"))
  # Use Switch to label
  dat$Candidate[1] <- switch(dat$cand_race[1],
    "1" = paste0("Latino Candidate: ", dat$Candidate[1]),
    "2" = paste0("Black Candidate: ", dat$Candidate[1]),
    "3" = paste0("Asian Candidate: ", dat$Candidate[1]),
    "4" = paste0("White Candidate: ", dat$Candidate[1])
  )
  dat$Candidate[2] <- switch(dat$cand_race[2],
    "1" = paste0("Latino Candidate: ", dat$Candidate[2]),
    "2" = paste0("Black Candidate: ", dat$Candidate[2]),
    "3" = paste0("Asian Candidate: ", dat$Candidate[2]),
    "4" = paste0("White Candidate: ", dat$Candidate[2])
  )
  message(paste("MC1: ", dat$Candidate[1], "\n"))
  message(paste("WC1: ", dat$Candidate[2], "\n"))
  # Order Columns to 2 minority columns first two, whites/nons last 2
  if (4 %in% group_race) {
    col_pos <- which(group_race == 4)
    if (col_pos == 2) {
      whites <- dat[, c(1, 4:5, ncol(dat))]
    } else {
      whites <- dat[, c(1, 2:3, ncol(dat))]
    }
  }
  if (3 %in% group_race) {
    col_pos <- which(group_race == 3)
    if (col_pos == 2) {
      asians <- dat[, c(1, 4:5, ncol(dat))]
    } else {
      asians <- dat[, c(1, 2:3, ncol(dat))]
    }
  }
  if (2 %in% group_race) {
    col_pos <- which(group_race == 2)
    if (col_pos == 2) {
      blacks <- dat[, c(1, 4:5, ncol(dat))]
    } else {
      blacks <- dat[, c(1, 2:3, ncol(dat))]
    }
  }
  if (1 %in% group_race) {
    col_pos <- which(group_race == 1) # Assumes 2 column groups
    if (col_pos == 2) {
      latinos <- dat[, c(1, 4:5, ncol(dat))]
    } else {
      latinos <- dat[, c(1, 2:3, ncol(dat))]
    }
  }
  # Paste Minority group with White groups; will only be one of black asian latino
  if (exists("asians")) { # Asians
    dat <- cbind(asians[, 1:3], whites[, 2:3])
  } else if (exists("latinos")) { # Latinos
    dat <- cbind(latinos[, 1:3], whites[, 2:3])
  } else {
    dat <- cbind(blacks[, 1:3], whites[, 2:3])
  }

  # 2 x 2 #
  min_cand <- dat[1, ] # Assuming
  white_cand <- dat[2, ]

  # Congruence

  # Row 1 -- Min
  ei_mv_minus_wv_for_mc <- as.numeric(min_cand[2] - min_cand[4])
  rc_mv_minus_wv_for_mc <- as.numeric(min_cand[3] - min_cand[5])
  cong_mv_minus_wv_for_mc <- 1 - (abs(ei_mv_minus_wv_for_mc - rc_mv_minus_wv_for_mc) /
    abs(mean(c(ei_mv_minus_wv_for_mc, rc_mv_minus_wv_for_mc))))

  row1 <- round(c(ei_mv_minus_wv_for_mc, rc_mv_minus_wv_for_mc, cong_mv_minus_wv_for_mc), 3)

  # Row 2 - Minority Candidate prefered by Minority Voters
  ei_mc_prefer <- as.character(ifelse(min_cand[2] > white_cand[2], "Yes", "No"))
  rc_mc_prefer <- as.character(ifelse(min_cand[3] > white_cand[3], "Yes", "No"))
  cong_pref <- ifelse(base::all.equal(ei_mc_prefer, rc_mc_prefer), 1, 0)

  row2 <- c(ei_mc_prefer, rc_mc_prefer, cong_pref)

  # Row 3 - Minority Candidate Preference Rate
  ei_min_cand_pref_rate <- as.numeric((min_cand[2] - white_cand[2]) / 2)
  rc_min_cand_pref_rate <- as.numeric((min_cand[3] - white_cand[3]) / 2)
  cong_min_cand_pref <- 1 - (abs(ei_min_cand_pref_rate - rc_min_cand_pref_rate) /
    abs(mean(c(ei_min_cand_pref_rate, rc_min_cand_pref_rate))))

  row3 <- round(c(ei_min_cand_pref_rate, rc_min_cand_pref_rate, cong_min_cand_pref), 3)

  # Row 4/5 -- Minority Candidate Blockd by White Voters
  ei_min_cand_block <- as.numeric((min_cand[4] - white_cand[4]) / 2)
  rc_min_cand_block <- as.numeric((min_cand[5] - white_cand[5]) / 2)
  cong_min_cand_block <- 1 - (abs(ei_min_cand_block - rc_min_cand_block) /
    abs(mean(c(ei_min_cand_block, rc_min_cand_block))))

  row5 <- round(c(ei_min_cand_block, rc_min_cand_block, cong_min_cand_block), 3)

  ei_block_yes <- ifelse(ei_min_cand_block < 0, "Yes", "No")
  rc_block_yes <- ifelse(rc_min_cand_block < 0, "Yes", "No")
  cong_block_score <- ifelse(base::all.equal(ei_block_yes, rc_block_yes), 1, 0)

  row4 <- c(ei_block_yes, rc_block_yes, cong_block_score)

  # Combine Minority scors
  all_mc <- rbind(row1, row2, row3, row4, row5)

  # Row 6: Model congruence
  row6 <- c("", "", mean(as.numeric(all_mc[, 3]))) # For adding total congruence
  all_mc <- rbind(all_mc, row6)
  colnames(all_mc) <- c("EI", "RxC", "Congruence")
  row.names(all_mc) <- c(
    "MV1-WV for MC1", "MC1 preferred by MV1", "MC1 preference rate",
    "MC1 blocked by WV", "MC1 block rate", "MC Model Congruence"
  )

  # Put rows 1-6 and 7-12 together; need to adjust the datatype, first
  all_mc <- data.frame(all_mc)
  all_mc$EI <- as.character(all_mc$EI)
  all_mc$RxC <- as.character(all_mc$RxC)
  all_mc$Congruence <- as.character(all_mc$Congruence)
  all_wc <- all_mc
  row.names(all_wc) <- c(
    "MV1-WV for WC1", "WC1 preferred by WV1", "WC1 preference rate",
    "WC1 blocked by MV1", "WC1 block rate", "WC Model Congruence"
  )

  # Replace coefficient signs for "white candidate"
  for (i in c(1, 3, 5)) {
    for (j in 1:2) {
      all_wc[i, j] <- as.numeric(all_wc[i, j]) * -1
    }
  }
  # Put it all back together
  mc_wc <- rbind(all_mc, all_wc)
  sw1 <- mc_wc[9, ] # Get a little funky
  sw2 <- mc_wc[11, ]
  mc_wc[9, ] <- sw2
  mc_wc[11, ] <- sw1
  final_row <- c("", "", mean(as.numeric(mc_wc[6, 3]), as.numeric(mc_wc[12, 3]))) # For adding total congruence

  # Output
  out <- rbind(mc_wc, "Total Model Congruence Score" = final_row)

  return(out)
}
