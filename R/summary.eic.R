#' Print a summary of an eiCompare object
#'
#' @param eic_object An eiCompare object, outputted from ei_iter() or ei_rxc()
#'
#' @export
#'
#' @return A nicely formatted dataframe for printing results
summary.eic <- function(eic_object) {
  estimates <- eic_object$estimates
  estimates <- data.frame(lapply(
    estimates,
    function(y) if (is.numeric(y)) round(y, 4) else y
  ))
  to_print <- get_md_bayes_gen_output(estimates)
  print(to_print)
}
