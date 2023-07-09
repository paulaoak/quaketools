#' @title Log-likelihood of GPD with variable threshold under the reparameterisation
#' of Mmax and mean.
#'
#' @description Function to evaluate the (negative) log-likelihood of GPD data
#' with variable threshold.
#' IMPORTANT: If you don't want a variable threshold just provide parameter v to
#' be v = u.
#'
#' @param mmax_mean GPD parameters mmax and mean at threshold u, that is the
#' the maximum (upper-end-point) and mean excess, respectively for the GPD.
#' @param u (Low) latent threshold of continuous GPD for which sigxi is given.
#' @param v Vector of latent threshold values for each observation.
#' @param x Vector of observed values.
#' @param negative Logical. Return negative log-likelihood?
#'
#' @return (Negative) log-likelihood of GPD data with variable threshold.
#'
#' @examples
#' #simulate data
#' v <- rep(c(1.6,1.2, 1.1), each = 50)
#' x <- rgpd_mmax_mean(n = 150,mmax = 10, mean = 5, shift = v)
#' llh_gpd_mmax_mean_varu(mmax_mean = c(11, 1), u = 1, v= v, x=x)
#' llh_gpd_mmax_mean_varu(mmax_mean = c(11, 5), u = 1, v= v, x=x)
#'
#' @export
llh_gpd_mmax_mean_varu <- function(mmax_mean, u, v, x, negative = FALSE){

  mmax = mmax_mean[1] - u
  mean = mmax_mean[2] - u
  # Check inputs
  stopifnot(exprs = {
    is.numeric(u)
    length(u) == 1
    is.numeric(mmax_mean)
    all(mmax > mean)
    all(mean > 0)
    length(mmax_mean)==2 #unique shape and scale_u parameters
    is.logical(negative)
    u <= min(v)
  })

  # Obtain shape and scale parameters in terms of Mmax and mean excesses
  xi = mean / (mean - mmax)
  sig = mean * mmax / (mmax - mean)

  # Likelihood
  llh <- llh_gpd_varu(sigxi = c(sig, xi), u, v, x, negative = negative)

  return((-1)^negative * llh)

}
