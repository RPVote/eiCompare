## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  warning = FALSE,
  comment = "#>"
)

library(eiCompare)

data("corona")

## -----------------------------------------------------------------------------
head(corona)

## -----------------------------------------------------------------------------
print(dim(corona))
names(corona)

## -----------------------------------------------------------------------------
corona$pct_hisp + corona$pct_non_lat == 1

## ----eval=FALSE---------------------------------------------------------------
#  cand_cols <- c("pct_husted", "pct_spiegel", "pct_ruth", "pct_button", "pct_montanez", "pct_fox")
#  race_cols <- c("pct_hisp", "pct_asian", "pct_white")
#  totals_col <- "totvote"
#  
#  # Run without parallization
#  start_time <- Sys.time()
#  results_test <- ei_iter(corona, cand_cols, race_cols, totals_col)
#  (end_time <- Sys.time() - start_time)

## ----eval=FALSE---------------------------------------------------------------
#  # Run with paralleization
#  start_time <- Sys.time()
#  results_test <- ei_iter(corona, cand_cols, race_cols, totals_col, par_compute = TRUE)
#  (end_time <- Sys.time() - start_time)

## ----echo=FALSE, out.width='100%'---------------------------------------------

knitr::include_graphics("para_benchmark_box_nsamples.png")

