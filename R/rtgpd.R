#' @title The Tapered Generalised Pareto Distribution
#'
#' @description Functions for working with the tapered generalised Pareto distribution:
#'
#' * `ptgpd()` gives the cumulative distribution function,
#' * `rtgpd()` generates random variates.
#'
#' The above functions use the \eqn{(\sigma,\xi)} parametrisation of the
#' generalised Pareto part of the distribution.
#'
#' Append "`_rd`" to the rtgpd function to sample observations accounting for
#' rounding, e.g. `rtgpd_rd()`.
#'
#' @details
#' Any shape values less than `shape_tolerance` are drawn from an exponential
#' distribution using the inverse CDF method. This is mathematically equivalent
#' to using `rexp()` but the seed is not handled in the same way.
#'
#' @author Paula Cordero Encinar
#'
#' @param n 	Number of random variates to generate.
#' @param scale Vector of scale parameters, \eqn{\sigma > 0}.
#' @param shape Vector of shape parameters, \eqn{\xi \in \mathbb{R}}.
#' @param shift  Vector of threshold parameters, \eqn{\mu \in \mathbb{R}}.
#' @param scale_taper  Vector of scale parameters for the taper function,
#' \eqn{\theta > 0}.
#' @param shape_tolerance Not intended for standard use. Scalar value, such that
#'  when `abs(shape) < shape_tolerance`, values are simulated from the limiting
#'  exponential distribution.
#' @return Vector of sampled values from generalised Pareto distribution.
#'
#' @examples
#' rtgpd(n = 5, scale = 1, shape = 0, shift = 0, scale_taper = 2)
#' rtgpd(n = 5, scale = 1:5, shape = 0.1, shift = 0, scale_taper = 10)
#' rtgpd(n = 5, scale = 1, shape = 0.1 * 1:5, shift = 0, scale_taper = 3)
#' rtgpd(n = 5, scale = 1, shape = 0, shift = 1:5, scale_taper = 10)
#' rtgpd(n = 5, scale = 1, shape = 0, shift = 0, scale_taper = 5:10)
#'
#' @export
rtgpd <- function(n, scale = 1, shape = 0, shift = 0, scale_taper = 2, shape_tolerance = 1e-10){

  # Check inputs
  input_lengths <- c(length(scale), length(shape), length(shift), length(scale_taper))
  stopifnot(exprs = {
    all(scale > 0)
    length(scale) >= 1
    length(shape) >= 1
    length(shift) >= 1
    length(scale_taper) >= 1
    length(shape_tolerance) == 1
    shape_tolerance >= 0
  })

  # Ensure scale, shape and shift are of same length.
  if ((length(scale) != n)) { scale <- rep(scale, length.out = n) }
  if ((length(shape) != n)) { shape <- rep(shape, length.out = n) }
  if ((length(shift) != n)) { shift <- rep(shift, length.out = n) }
  if ((length(scale_taper) != n)) { scale_taper <- rep(scale_taper, length.out = n) }

  # Simulate sample
  U <- stats::runif(n)
  V <- stats::runif(n)
  sample_1 <- shift + (scale / shape) * ((1 - U)^(-shape) - 1)
  sample_2 <- shift - scale_taper * log(V)
  sample <- pmin(sample_1, sample_2)

  # Check for and correct any values from exponential distribution (xi ≈ 0)
  which_shape_near_zero <- which(abs(shape) <= shape_tolerance)
  n_shape_near_zero <- length(which_shape_near_zero)

  if (n_shape_near_zero > 0) {
    exp_samples_1 <- shift - scale * log(U[which_shape_near_zero])
    exp_samples_2 <- shift - scale_taper * log(V[which_shape_near_zero])
    sample[which_shape_near_zero] <- pmin(exp_samples_1, exp_samples_2)
  }

  # Show warning if latent parameters of the GPD are not of the same length and the ones of different lengths are not unit vectors
  show_warning = !(sum(input_lengths) %in% c(4, n+3, 2*n+2, 3*n+1, 4*n))
  if (show_warning) warning('Scale, shape and shift parameter vectors are not of the same length; shorter vectors are recycled')

  return(sample)
}

