## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

options(scipen = 999, digits = 4)

## -----------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(eiCompare)
  library(dplyr)
})

data("gwinnett")
head(gwinnett)

## -----------------------------------------------------------------------------
cands <- c("kemp", "abrams", "metz")
races <- c("white", "black", "hispanic", "other")
total <- "turnout"
id <- "precinct"

## -----------------------------------------------------------------------------
gwinnett <- resolve_missing_vals(
  data = gwinnett,
  cand_cols = cands,
  race_cols = races,
  totals_col = total,
  na_action = "DROP"
)

## -----------------------------------------------------------------------------
gwinnett <- dedupe_precincts(
  data = gwinnett,
  id_cols = id
)

## -----------------------------------------------------------------------------
gwinnett %>%
  filter(duplicate)

## -----------------------------------------------------------------------------
missing_inds <- which(gwinnett$duplicate)
columns_to_add <- c(total, cands)
gwinnett[missing_inds[1], columns_to_add] <-
  gwinnett[missing_inds[1], columns_to_add] +
  gwinnett[missing_inds[2], columns_to_add]
gwinnett <- gwinnett[-missing_inds[2], ]
gwinnett[missing_inds[1], ]

## -----------------------------------------------------------------------------
gwinnett <- dedupe_precincts(
  data = gwinnett,
  id_cols = id
)

## -----------------------------------------------------------------------------
gwinnett_ei <- stdize_votes_all(
  data = gwinnett,
  cand_cols = cands,
  race_cols = races,
  totals_col = total
)
head(gwinnett_ei)

## -----------------------------------------------------------------------------
cand_sums <- sum_over_cols(data = gwinnett_ei, cols = cands)
race_sums <- sum_over_cols(data = gwinnett_ei, cols = races)
table(cand_sums, race_sums)

## ----fig.height = 5, fig.width = 7, fig.align = "center"----------------------
plot_bivariate(
  data = gwinnett_ei,
  cand_cols = cands,
  race_cols = races
)

## -----------------------------------------------------------------------------
race_cand_cors(
  data = gwinnett_ei,
  cand_cols = cands,
  race_cols = races
)

## -----------------------------------------------------------------------------
ei_results_iter <- ei_iter(
  data = gwinnett_ei,
  cand_cols = cands,
  race_cols = races,
  totals_col = total,
  name = "Iter"
)
summary(ei_results_iter)

## -----------------------------------------------------------------------------
ei_results_rxc <- ei_rxc(
  data = gwinnett_ei,
  cand_cols = cands,
  race_cols = races,
  totals_col = total,
  ntunes = 1,
  samples = 5000,
  thin = 1,
  name = "RxC"
)
summary(ei_results_iter, ei_results_rxc)

## ----fig.height = 6, fig.width = 8, fig.align = "center"----------------------
plot(ei_results_iter, ei_results_rxc)

