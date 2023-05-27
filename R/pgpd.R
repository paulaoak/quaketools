#' @inherit rgpd title description
#'
#' @details
#' Any shape values less than `shape_tolerance` are calculated for an exponential
#' distribution using `dexp()`.
#'
#' @author Zak Varty
#'
#' @inheritParams rgpd
#' @param q Vector of quantiles.
#'
#' @return Cumulative distribution function value: \eqn{\Pr(X \leq q)}.
#'
#' @examples
#' pgpd(q = seq(-1, 2), shape = 0, scale = 1)
#' pgpd(q = 1, shape = c(0,-1), scale = c(0.1,1))
#'
#' @importFrom stats pexp
#' @export
pgpd <- function(q, scale = 1, shape = 0, shift = 0, shape_tolerance = 1e-10){

  # Check inputs
  input_lengths <- c(length(q), length(scale), length(shape), length(shift))
  n <- max(input_lengths)
  stopifnot(exprs = {
    all(scale > 0)
    length(shape_tolerance) == 1
    shape_tolerance >= 0
  })

  # Ensure q, scale, shape and mu are of same length.
  if ((length(scale) < n) & (n > 1)) { scale <- rep(scale, length.out = n) }
  if ((length(shape) < n) & (n > 1)) { shape <- rep(shape, length.out = n) }
  if ((length(shift) < n) & (n > 1)) { shift <- rep(shift, length.out = n) }
  if ((length(q) < n) & (n > 1)) {q <- rep(q, length.out = n)}

  # Calculate probabilities
  p <- (1 - (1 + (shape * (q - shift)) / scale)^(-1 / shape))

  # Correct probabilities below threshold
  value_below_threshold <- 0
  p[q < shift] <- value_below_threshold

  # Correct probabilities above upper end point
  value_above_UEP <- 1
  is_above_UEP <- (shape < 0) & (q >= (shift - scale / shape))
  p[is_above_UEP] <- value_above_UEP

  # Check for and correct any values from exponential distribution (xi ≈ 0)
  which_shape_near_zero <- which(abs(shape) <= shape_tolerance)
  n_shape_near_zero <- length(which_shape_near_zero)

  if (n_shape_near_zero > 0) {
    exp_qs <- q[which_shape_near_zero] - shift[which_shape_near_zero]
    exp_rates <- 1 / scale[which_shape_near_zero]

    exp_ps <- stats::pexp(q = exp_qs, rate = exp_rates)
    p[which_shape_near_zero] <- exp_ps
  }
  
  # Show warning if quantiles, shape, scale and shift inputs are not of the same length and the ones of different lengths are not unit vectors
  show_warning = !(sum(input_lengths) %in% c(n+3, 2*n+2, 3*n+1, 4*n))
  warning('Quantile vector, scale, shape and shift parameter vectors are not of the same length; shorter vectors are recycled', call. = show_warning, immediate. = FALSE, noBreaks. = FALSE,
          domain = NULL)

  return(p)
}
