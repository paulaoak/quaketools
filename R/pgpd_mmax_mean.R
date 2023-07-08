#' @inherit rgpd title description
#'
#'
#' @author Paula Cordero Encinar
#'
#' @inheritParams rgpd_mmax_mean
#' @param q Vector of quantiles.
#'
#' @return Cumulative distribution function value: \eqn{\Pr(X \leq q)}.
#'
#' @examples
#' pgpd_mmax_mean(q = seq(-1, 2), mmax = 10, mean = 5)
#' pgpd_mmax_mean(q = 1, mmax = c(20,10), mean = c(5,6))
#'
#' @export
pgpd_mmax_mean <- function(q, mmax = 1, mean = 0, shift = 0){

  # Check inputs
  input_lengths <- c(length(q), length(mmax), length(mean), length(shift))
  n <- max(input_lengths)
  stopifnot(exprs = {
    all(mmax > mean)
    all(shift < mean)
    length(q) >= 1
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

  p <- pgpd(q = q, scale = scale, shape = shape, shift = shift)

  return(p)
}
