#' precinct_agg_combine
#' 
#' Combines voter file surname geocoded data into aggregated precinct counts.
#' To be performed on data object resulting from successful completion of
#' bisg_prep_race_predict() function.
#' 
#' 
#' @param dat data.frame() object
#' @param precinct Character of precinct column name. Default = "precinct",
#' which is what is output from bisg_prep_race_predict()
#' @return Aggregated dataset of nrow() precinct size, including racial size
#' precinct estimates. Dataset suitable for EI/RxC.
#' @author Loren Collingwood <loren.collingwood@@ucr.edu>
#' @examples
#' 
#' 
#' @export precinct_agg_combine
precinct_agg_combine <- function(dat, precinct = "precinct") {

  # Execute Function output into list format output #
  precinct_agg <- function(list_obj) {

    # Sum up Race probabilities #
    pred <- round(apply(list_obj[, grep("pred.", colnames(list_obj))], 2, sum), 0)

    # Adjust for no race prediction #
    sum_no_pred <- table(apply(list_obj[, grep("pred.", colnames(list_obj))], 1, sum))["0"]
    if (!is.na(sum_no_pred)) {
      pred <- c(pred, pred.none = sum_no_pred)
      names(pred)[length(pred)] <- "pred.none"
    } else { #

      pred <- c(pred, pred.no_name = 0)
      names(pred)[length(pred)] <- "pred.none"
    }

    pred <- data.frame(t(pred))
    pred <- data.frame(total = sum(pred), pred)
    colnames(pred) <- c(
      "total_agg", "pred.whi_agg", "pred.bla_agg",
      "pred.his_agg", "pred.asi_agg", "pred.oth_agg",
      "pred.none_agg"
    )

    # Percentages ; drop the 'none' predictions #
    pred$pct_whi_agg <- with(pred, pred.whi_agg / (total_agg - pred.none_agg))
    pred$pct_bla_agg <- with(pred, pred.bla_agg / (total_agg - pred.none_agg))
    pred$pct_his_agg <- with(pred, pred.his_agg / (total_agg - pred.none_agg))
    pred$pct_asi_agg <- with(pred, pred.asi_agg / (total_agg - pred.none_agg))
    pred$pct_other_agg <- with(pred, pred.oth_agg / (total_agg - pred.none_agg))
    pred$pct_min_agg <- with(pred, 1 - pct_whi_agg)
    return(pred)
  }
  # Split Data on Precinct; n=10 precincts (for most)
  bisg_split <- split(dat, dat[, precinct])
  precinct_data <- lapply(bisg_split, precinct_agg) # apply above function
  precinct_data <- rbindlist(precinct_data)
  # precinct_data[is.na(precinct_data)] <- 0 # Fill in missing with 0 (like "race/other pred")

  return(precinct_data)
}
