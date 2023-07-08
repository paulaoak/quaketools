#' @inherit rgpd title description
#'
#'
#' @author Paula Cordero Encinar
#'
#' @inheritParams rgpd_mmax_mean
#' @param p Vector of probabilities relating to the desired GPD quantiles.
#'
#' @return Quantile function value: \eqn{x_p} such that \eqn{\Pr(X \leq x_p) = p}.
#'
#' @examples
#' qgpd_mmax_mean(p = 0.9, mmax = 10, mean = 1, shift = 0)
#' qgpd_mmax_mean(p = 0.9, mmax = 10, mean = 5, shift = 1)
#'
#' qgpd_mmax_mean(p = c(0.8, 0.9, 1), mmax = 10, mean = 4, shift = 0)
#' qgpd_mmax_mean(p = c(0.8, 0.9, 1), mmax = 10, mean = 8, shift = 0)
#' qgpd_mmax_mean(p = c(0.8, 0.9, 1), mmax = 10, mean = 1, shift = 0)
#'
#' qgpd_mmax_mean(p = 0.1, mmax = c(11,22,33), mean = 5, shift = c(1,2,3))
#'
#' @export
qgpd_mmax_mean <- function(p, mmax = 2, mean = 1, shift = 0){

  # Check inputs
  input_lengths <- c(length(p), length(mmax), length(mean), length(shift))
  n <- max(input_lengths)
  stopifnot(exprs = {
    all(mean > shift)
    all(mean < mmax)
    all(p >= 0)
    all(p <= 1)
    length(p) >= 1
    length(mmax) >= 1
    length(mean) >= 1
    length(shift) >= 1
  })

  # Substract shift to obtain Mmax and mean excesses
  mmax_excess = mmax - shift
  mean_excess = mean - shift

  # Obtain shape and scale parameters in terms of Mmax and mean excesses
  shape = mean_excess / (mean_excess - mmax_excess)
  scale = mean_excess * mmax_excess / (mmax_excess - mean_excess)

  q <- qgpd(p = p, scale = scale, shape = shape, shift = shift)

  return(q)
}
