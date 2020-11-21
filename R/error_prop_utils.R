#' Calculate error on the product of the terms in the numerator of the BISG
#' estimate.
#'
#' This serves as a helper function for computing the BISG error estimate.
#'
#' @param p_g_r 
#' @param p_r_s
#' @param p_g_r_err
#' @param p_r_s_err
compute_product_err <- function(
    p_g_r, p_r_s, p_g_r_err, p_r_s_err
) {
  error <- p_r_s^2 * p_g_r_err + p_g_r^2 * p_r_s_err
  return(error)
}


#' Calculate error on the product of the terms in the numerator of the BISG
#' estimate.
#'
#' This serves as a helper function for computing the BISG error estimate.
#'
#' @param p_g_r 
#' @param p_r_s
#' @param p_g_r_err
#' @param p_r_s_err
compute_bisg_err <- function(
    p_g_r, p_r_s, p_g_r_err, p_r_s_err
) {
  product <- p_g_r * p_r_s
  # Calculate error on product term
  product_err <- compute_product_err(p_g_r, p_r_s, p_g_r_err, p_r_s_err)
  # Calculate sum of product term (normalization constant)
  norm <- rowSums(product)
  norm_err <- rowSums(product_err)
  final_err <- product_err / norm^2 + product^2 * (norm_err / norm^4)
  return(final_err)
}