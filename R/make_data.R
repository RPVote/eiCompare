#' Create a dataframe with NA values for racial and candidate counts.
#' 
#' @param ncand The number of candidates to include
#' @param nrace The number of race/ethnicitiesto include
#' @param nrow The number of rows for the dataframe
#' 
#' @return A dataframe with columns for each candidate and race, all with NAs

empty_ei_df <- function(ncand = 2, nrace = 2, nrow = 2) {
  cands <- paste0('c', c(1:ncand))
  races <- paste0('r', c(1:nrace))
  if (ncand == 0) {
    df <- as.data.frame(matrix(nrow = nrow, ncol = (nrace)))
    names(df) <- c(races)
  } else if (nrace == 0) {
    df <- as.data.frame(matrix(nrow = nrow, ncol = (ncand)))
    names(df) <- c(cands)
  } else {
    df <- as.data.frame(matrix(nrow = nrow, ncol = (ncand + nrace)))
    names(df) <- c(cands, races)
  }
  return(df)
}
