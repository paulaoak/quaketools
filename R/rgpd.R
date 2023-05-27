#' @title The Generalised Pareto Distribution
#'
#' @description Functions for working with the generalised Pareto distribution:
#'
#' * `dgpd()` gives the probability density function,
#' * `pgpd()` gives the cumulative distribution function,
#' * `qgpd()` gives the quantile function,
#' * `rgpd()` generates random variates.
#'
#' The above functions use the \eqn{(\sigma,\xi)} parametrisation of the
#' generalised Pareto distribution.
#'
#' Append "`_nu`" to the function name to use the \eqn{(\nu,\xi)} parametrisation
#' instead, e.g. `dgpd_nu()`.
#'
#' @details
#' Any shape values less than `shape_tolerance` are drawn from an exponential
#' distribution using the inverse CDF method. This is mathematically equivalent
#' to using `rexp()` but the seed is not handled in the same way.
#'
#' @author Zak Varty
#'
#' @param n 	Number of random variates to generate.
#' @param scale Vector of scale parameters, \eqn{\sigma > 0}.
#' @param shape Vector of shape parameters, \eqn{\xi \in \mathbb{R}}.
#' @param shift  Vector of threshold parameters, \eqn{\mu \in \mathbb{R}}.
#' @param shape_tolerance Not intended for standard use. Scalar value, such that
#'  when `abs(shape) < shape_tolerance`, values are simulated from the limiting
#'  exponential distribution.
#' @return Vector of sampled values from generalised Pareto distribution.
#'
#' @examples
#' rgpd(n = 5, scale = 1, shape = 0, shift = 0)
#' rgpd(n = 5, scale = 1:5, shape = 0.1, shift = 0)
#' rgpd(n = 5, scale = 1, shape = 0.1 * 1:5, shift = 0)
#' rgpd(n = 5, scale = 1, shape = 0, shift = 1:5)
#'
#' @export
rgpd <- function(n, scale = 1, shape = 0, shift = 0, shape_tolerance = 1e-10){

  # Check inputs
  stopifnot(exprs = {
    all(scale > 0)
    length(shape_tolerance) == 1
    shape_tolerance >= 0
  })

  # Ensure scale, shape and shift are of same length.
  if ((length(scale) < n) & (n > 1)) { scale <- rep(scale, length.out = n) }
  if ((length(shape) < n) & (n > 1)) { shape <- rep(shape, length.out = n) }
  if ((length(shift) < n) & (n > 1)) { shift <- rep(shift, length.out = n) }

  # Simulate sample
  U <- stats::runif(n)
  sample <- shift + (scale / shape) * ((1 - U)^(-shape) - 1)

  # Check for and correct any values from exponential distribution (xi ≈ 0)
  which_shape_near_zero <- which(abs(shape) <= shape_tolerance)
  n_shape_near_zero <- length(which_shape_near_zero)

  if (n_shape_near_zero > 0) {
    exp_samples <- -scale * log(U[which_shape_near_zero])
    sample[which_shape_near_zero] <- shift + exp_samples
  }

  # Show warning if latent parameters of the GPD are not of the same length and the ones of different lengths are not unit vectors
  show_warning = !(sum(input_lengths) %in% c(n+2, 2*n+1, 3*n))
  if (show_warning) warning('Scale, shape and shift parameter vectors are not of the same length; shorter vectors are recycled')
  
  return(sample)
}
