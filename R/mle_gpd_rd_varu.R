#' @title MLE for rounded GPD data with variable threshold.
#'
#' @description This function computes the maximum likelihood estimators for the
#' for rounded GPD data with variable threshold. In addition, the function can
#' also return the log-likelihood value as well as a numerical
#' estimation of the hessian at the MLE.
#'
#' @param sigxi Initial parameter values for optimisation (latent gpd parameters at threshold u).
#' @param u (Low) threshold of latent gpd at which to return optimal parameter values.
#' @param x Vector of rounded gpd observations.
#' @param v Vector of latent gpd thresholds.
#' @param to_nearest Level to which observed values are rounded.
#' @param llh_val Logical. Return the log-likelihood value at mle?
#' @param hessian Logical. Return numerically estimated hessian at mle?
#' @param method The method to be used. See ?optim for more details.
#' @param maxit Maximum number of iterations to be passed to optim function.
#' @param ... additional parameters to be passed to optim function.
#'
#' @return Vector of parameter mles or list containing mles, log-likelihood value and hessian.
#'
#' @examples
#' #simulate data
#' v <- rep(c(1.6,1.2, 1.1), each = 50)
#' x <- rgpd_rd(n = 150,scale = 1.1, shape = 0.2, shift = v, to_nearest = 0.1)
#' mle_gpd_rd_varu(sigxi = c(1, 0.01), u = 1, v= v, x=x)
#'
#' @importFrom stats optim
#' @export
mle_gpd_rd_varu <- function(sigxi, u, v, x, to_nearest = 0.1, llh_val = TRUE, hessian = FALSE, method =  "Nelder-Mead", maxit = 10000, ...){

  # Check inputs
  stopifnot(exprs = {
    is.numeric(u)
    length(u) == 1
    is.numeric(sigxi)
    length(sigxi)==2 #unique shape and scale_u parameters
    is.numeric(to_nearest)
    length(to_nearest) == 1
    is.logical(llh_val)
    is.logical(hessian)
  }
  )

  # Latent GPD parameters
  sig_u_0 <- sigxi[1]
  xi_0 <- sigxi[2]
  sig_v_0 <- sig_u_0 + xi_0 * (v-u)
  delta <- to_nearest/2

  # Check valid starting point
  condition <- (xi_0 < 0) && (max(x) >= (u - sig_u_0 / xi_0 + delta))
  if(condition){stop('Invalid starting point. Data above upper end point of distribution.')}
  stopifnot(all(sig_v_0>0))
  stopifnot(sig_u_0>0)

  # Numerically minimise negative log-likelihood
  estimate <- optim(fn = llh_gpd_rd_varu,
                    par = sigxi,
                    u = u,
                    v = v,
                    x = x,
                    to_nearest = to_nearest,
                    negative = TRUE,
                    hessian = hessian,
                    method = method,
                    control = list(maxit = maxit, ...))

  if(estimate$convergence)
    warning("optimization may not have succeeded")

  # Format output
  if(!llh_val & !hessian){out <-  estimate$par} else {out <- list(params = estimate$par)}
  if(llh_val) out$loglik <- -estimate$value
  if(hessian) out$hessian <- estimate$hessian

  return(out)
}
