#' Calculate error on the product of the terms in the numerator of the BISG
#' estimate.
#'
#' This serves as a helper function for computing the BISG error estimate.
#'
#' @param p_g_r A tibble denoting the probability of a geography, given race.
#'  Each row in the tibble denotes a voter.
#' @param p_r_s A tibble denoting the probability of a racial group, given a
#'  surname. Each row in the tibble denotes a voter.
#' @param p_g_r_err A tibble denoting the error on the probability of a
#'  geography, conditioned on racial group. Same size as p_g_r.
#' @param p_r_s_err A tibble denoting the error on the probability of racial
#'  group, conditioned on race. Same size as p_r_s.
#'
#' @return The error on p_g_r times p_r_s.
compute_product_err <- function(p_g_r, p_r_s, p_g_r_err, p_r_s_err) {
  err <- p_r_s^2 * p_g_r_err + p_g_r^2 * p_r_s_err
  return(err)
}


#' Calculate error on the BISG output, given input probability distributions.
#'
#' This function calculates the error on the posterior probability given the
#' two distributions - probability of geography conditioned on race, and
#' probability of race conditioned on surname - that are used to perform BISG.
#'
#' @param p_g_r A tibble denoting the probability of a geography, given race.
#'  Each row in the tibble denotes a voter.
#' @param p_r_s A tibble denoting the probability of a racial group, given a
#'  surname. Each row in the tibble denotes a voter.
#' @param p_g_r_err A tibble denoting the error on the probability of a
#'  geography, conditioned on racial group. Same size as p_g_r.
#' @param p_r_s_err A tibble denoting the error on the probability of racial
#'  group, conditioned on race. Same size as p_r_s.
#'
#' @return The error on the posterior probability.
compute_bisg_err <- function(p_g_r, p_r_s, p_g_r_err, p_r_s_err) {
  product <- p_g_r * p_r_s
  # Calculate error on product term
  product_err <- compute_product_err(p_g_r, p_r_s, p_g_r_err, p_r_s_err)
  # Calculate sum of product term (normalization constant)
  norm <- rowSums(product)
  norm_err <- rowSums(product_err)
  final_err <- product_err / norm^2 + product^2 * (norm_err / norm^4)
  return(final_err)
}