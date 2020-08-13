#' Print a summary of an eiCompare object
#'
#' @param object An eiCompare object, outputted from ei_iter() or ei_rxc()
#' @param ... Additional eiCompare objects to summarize
#' @return A nicely formatted dataframe for printing results
#' @export
summary.eiCompare <- function(object, ...) {
  objects <- list(object, ...)
  tables <- vector("list", length(objects))

  for (ii in seq_along(objects)) {
    object <- objects[[ii]]
    estimates <- object$estimates
    estimates <- data.frame(lapply(
      estimates,
      function(y) if (is.numeric(y)) round(y, 4) else y
    ))
    tables[[ii]] <- get_md_bayes_gen_output(estimates, tag = object$name)
  }

  race_groups <- names(tables[[1]])
  outputs <- list()

  for (ii in seq_along(race_groups)) {
    race <- race_groups[[ii]]
    outputs[[race]] <- data.frame(lapply(tables, function(x) x[[race]]))
  }
  print(outputs)
}
