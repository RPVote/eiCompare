#' mean_and_ci
#' 
#' Internal
#' 
#' 
#' @param cbind_dat cbind object
#' @param ci Credible intervals. Default: c(.025, .975)
#' @return Mean and credible interval
#' @author Loren Collingwood <loren.collingwood@@ucr.edu>
#' @examples
#' 
#'   
#'   # EXAMPLE: NOT RUN #
#'   
#' 
#' @export mean_and_ci
mean_and_ci <- function(cbind_dat, ci = c(.025, .975)) {
  # Add votes together
  total <- apply(cbind_dat, 1, sum)
  # Create vote filler
  v_fill <- matrix(NA, nrow = nrow(cbind_dat), ncol = ncol(cbind_dat))
  # Create mean, CI filler
  qtile <- matrix(NA, nrow = ncol(cbind_dat), ncol = 3)
  # Create percents for every vote column for 1 racial group
  for (j in 1:ncol(v_fill)) {
    # Divide each vote by the total
    v_fill[, j] <- cbind_dat[, j] / total
    # Mean, 95% confidence interval
    qtile[j, ] <- c(mean(v_fill[, j]), quantile(v_fill[, j], ci))
  }
  # Label Output #
  row.names(qtile) <- colnames(cbind_dat)
  colnames(qtile) <- c("Mean", "2.5", "97.5")
  # Return Mean, CI table
  return(qtile)
}
