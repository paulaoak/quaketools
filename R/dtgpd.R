#' @inherit rtgpd title description
#'
#' @details
#' Any shape values less than `shape_tolerance` are calculated for an exponential
#' distribution using `dexp()`.
#'
#' @author Paula Cordero Encinar
#'
#' @inheritParams rtgpd
#' @param x   Vector of values at which to evaluate the probability density function.
#' @param log Logical; if TRUE then log-density is returned.
#'
#' @return Vector of probability density function values at `x`.
#'
#' @examples
#' evaluation_points <- c(-1, 0, 0.5, 1, 1.9, 2.1, 5)
#' dtgpd(x = evaluation_points, scale = 1, shape = 0, scale_taper = 2)
#' dtgpd(x = evaluation_points, scale = 1, shape = 0.2, scale_taper = 2)
#' dtgpd(x = evaluation_points, scale = 1, shape = -0.2, scale_taper = 3)
#'
#' @export
dtgpd <- function(x, scale = 1, shape = 0, shift = 0, scale_taper = 2, shape_tolerance = 1e-10, log = FALSE){

  # Check inputs
  input_lengths <- c(length(x), length(scale), length(shape), length(shift), length(scale_taper))
  n <- max(input_lengths)
  stopifnot(exprs = {
    all(scale > 0)
    all(scale_taper > 0)
    length(x) >= 1
    length(scale) >= 1
    length(shape) >= 1
    length(shift) >= 1
    length(scale_taper) >= 1
    length(shape_tolerance) == 1
    shape_tolerance >= 0
    length(log) == 1
    log %in% c(TRUE, FALSE)
  })

  # Ensure x, scale, shape and mu are of same length.
  if ((length(scale) < n) & (n > 1)) { scale <- rep(scale, length.out = n) }
  if ((length(shape) < n) & (n > 1)) { shape <- rep(shape, length.out = n) }
  if ((length(shift) < n) & (n > 1)) { shift <- rep(shift, length.out = n) }
  if ((length(scale_taper) < n) & (n > 1)) { scale_taper <- rep(scale_taper, length.out = n) }
  if ((length(x) < n) & (n > 1)) {x <- rep(x, length.out = n)}

  # Calculate the GPD (log-)density at each point in x
  if (log) {
    out <- log(scale_taper ^ (-1) + scale ^ (-1) * pmax((1 + shape * (x - shift) / scale), 0) ^ (-1)) + (-1 / shape) * log(pmax((1 + shape * (x - shift) / scale), 0)) - (x - shift)/scale_taper
  } else {
    out <- (scale_taper ^ (-1) + scale ^ (-1) * pmax((1 + shape * (x - shift) / scale), 0) ^ (-1)) * pmax((1 + shape * (x - shift) / scale), 0) ^ (-1 / shape) * exp(-(x - shift) / scale_taper)
  }

  # Amend values below threshold
  out_of_support_value <- ifelse(log, yes = -Inf, no = 0)
  out[which(x < shift)] <- out_of_support_value

  # Amend values above upper endpoint (if it exists)
  out[which((shape < 0) & (x >= (shift - scale / shape)))] <- out_of_support_value

  # Check for and correct any values from exponential distribution (xi ≈ 0)
  which_shape_near_zero <- which(abs(shape) <= shape_tolerance)
  n_shape_near_zero <- length(which_shape_near_zero)

  if (n_shape_near_zero > 0) {
    exp_xs <- x[which_shape_near_zero] - shift[which_shape_near_zero]
    exp_rates <- 1 / scale[which_shape_near_zero] + 1 / scale_taper[which_shape_near_zero]
    exp_densities <- stats::dexp(x = exp_xs, rate = exp_rates, log = log)
    out[which_shape_near_zero] <- exp_densities
  }

  show_warning = !(sum(input_lengths) %in% c(n+4, 2*n+3, 3*n+2, 4*n+1, 5*n))
  if (show_warning) warning('Vector of values, scale, shape and shift parameter vectors are not of the same length; shorter vectors are recycled')

  return(out)
}
