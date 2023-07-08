#' @inherit rgpd title description
#'
#'
#' @author Paula Cordero Encinar
#'
#' @inheritParams rgpd_mmax_mean
#' @param x   Vector of values at which to evaluate the probability density function.
#' @param log Logical; if TRUE then log-density is returned.
#'
#' @return Vector of probability density function values at `x`.
#'
#' @examples
#' evaluation_points <- c(-1, 0, 0.5, 1, 1.9, 2.1, 5)
#' dgpd_mmax_mean(x = evaluation_points, mmax = 5, mean = 3)
#' dgpd_mmax_mean(x = evaluation_points, mmax = 10, mean = 4)
#'
#' @export
dgpd_mmax_mean <- function(x, mmax = 2, mean = 1, shift = 0, log = FALSE){

  # Check inputs
  input_lengths <- c(length(x), length(mmax), length(mean), length(shift))
  n <- max(input_lengths)
  stopifnot(exprs = {
    all(mean > shift)
    all(mean < mmax)
    length(x) >= 1
    length(mmax) >= 1
    length(mean) >= 1
    length(shift) >= 1
    length(log) == 1
    log %in% c(TRUE, FALSE)
  })

  # Substract shift to obtain Mmax and mean excesses
  mmax_excess = mmax - shift
  mean_excess = mean - shift

  # Obtain shape and scale parameters in terms of Mmax and mean excesses
  shape = mean_excess / (mean_excess - mmax_excess)
  scale = mean_excess * mmax_excess / (mmax_excess - mean_excess)

  out <- dgpd(x, scale = scale, shape = shape, shift = shift, log = log)

  return(out)
}

