#' @inherit rgpd title description
#'
#' @author Paula Cordero Encinar
#'
#' @param n 	Number of random variates to generate.
#' @param mmax Vector of mmax values without substracting the threshold, \eqn{M_{\max} > mean > shift}.
#' @param mean Vector of mean values without substracting the threshold, \eqn{M_{\max} > mean > shift}.
#' @param shift  Vector of threshold parameters, \eqn{\mu \in \mathbb{R}}.
#' @return Vector of sampled values from generalised Pareto distribution.
#'
#' @examples
#' rgpd_mmax_mean(n = 5, mmax = 3, mean = 1, shift = 0)
#' rgpd_mmax_mean(n = 5, mmax = 3:5, mean = 1, shift = 0)
#' rgpd_mmax_mean(n = 5, mmax = 10, shape = 0.5 * 1:5, shift = 0)
#' rgpd_mmax_mean(n = 5, scale = 10, shape = 5, shift = 1:4)
#'
#' @export
rgpd_mmax_mean <- function(n, mmax = 2, mean = 0.5, shift = 0){

  # Check inputs
  input_lengths <- c(length(mmax), length(mean), length(shift))
  stopifnot(exprs = {
    all(mean < mmax)
    all(mean > shift)
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

  sample <- rgpd(n = n, scale = scale, shape = shape, shift = shift)

  return(sample)
}
