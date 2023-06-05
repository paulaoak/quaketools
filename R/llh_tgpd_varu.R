#' @title Log-likelihood of Tapered GPD with variable threshold.
#'
#' @description Function to evaluate the (negative) log-likelihood of tapered
#' GPD data with variable threshold.
#'
#' @param sigxitheta Tapered GPD parameters at threshold u.
#' @param u (Low) latent threshold of continuous GPD for which sigxi is given.
#' @param v Vector of latent threshold values for each observation.
#' @param x Vector of observed values.
#' @param negative Logical. Return negative log-likelihood?
#'
#' @return (Negative) log-likelihood of tapered GPD data with variable threshold.
#'
#' @examples
#' #simulate data
#' v <- rep(c(1.6,1.2, 1.1), each = 50)
#' x <- rtgpd(n = 150, scale = 1.1, shape = 0.2, shift = v, scale_taper = 2)
#' llh_tgpd_varu(sigxitheta = c(0.1, -0.01, 0.2), u = 1, v= v, x=x)
#' llh_tgpd_varu(sigxitheta = c(0.1, -0.5, 0.2), u = 1, v= v, x=x)
#'
#' @export
llh_tgpd_varu <- function(sigxitheta, u, v, x, negative = FALSE){

  # Check inputs
  stopifnot(exprs = {
    is.numeric(u)
    length(u) == 1
    is.numeric(sigxitheta)
    length(sigxitheta)==3 #unique shape and scale_u parameters
    is.logical(negative)
    u <= min(v)
  })

  # Latent GPD parameters
  sig_u <- sigxitheta[1]
  xi <- sigxitheta[2]
  theta <- sigxitheta[3]
  sig_v <- sig_u + xi * (v-u) #threshold dependent scale parameter

  # Check x all be above v
  lep_fail <- any(x < v)
  if(lep_fail){stop('Lower endpoint failure:  !all(x > v).')}

  # Function body

  # Satisfy conditions to be a valid GPD distribution:
  #check all scale parameters are positive
  condition_1 <- sig_u <= 0
  condition_2 <- theta <= 0
  #check all x below upper end-point (UEP) (if it exists)
  condition_3 <- (xi < 0) && (max(x) >= (u - sig_u / xi))

  if(condition_1 || condition_2 || condition_3){
    llh <- -10e6
    return((-1)^negative * llh)}

  # Log-likelihood
  prob <- dtgpd(x = x, shape = xi, scale = sig_v, shift = v, scale_taper = theta)
  llh <- sum(log(prob))

  return((-1)^negative * llh)

}
