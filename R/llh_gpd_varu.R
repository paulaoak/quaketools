#' @title Log-likelihood of GPD with variable threshold.
#'
#' @description Function to evaluate the (negative) log-likelihood of GPD data
#' with variable threshold.
#'
#' @param sigxi GPD parameters at threshold u.
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
#' x <- rgpd(n = 150,scale = 1.1, shape = 0.2, shift = v)
#' llh_gpd_varu(sigxi = c(0.1, -0.01), u = 1, v= v, x=x)
#' llh_gpd_varu(sigxi = c(0.1, -0.5), u = 1, v= v, x=x)
#'
#' @export
llh_gpd_varu <- function(sigxi, u, v, x, negative = FALSE){

  # Check inputs
  stopifnot(exprs = {
    is.numeric(u)
    length(u) == 1
    is.numeric(sigxi)
    length(sigxi)==2 #unique shape and scale_u parameters
    is.logical(negative)
    u <= min(v)
  })

  # Latent GPD parameters
  sig_u <- sigxi[1]
  xi <- sigxi[2]
  sig_v <- sig_u + xi * (v-u) #threshold dependent scale parameter

  # Check x all be above v
  lep_fail <- any(x < v)
  if(lep_fail){stop('Lower endpoint failure:  !all(x > v).')}


  # Function body

  # Satisfy conditions to be a valid GPD distribution:
  #check all scale parameters are positive
  condition_1 <- sig_u <= 0
  #check all x below upper end-point (UEP) (if it exists)
  condition_2 <- (xi < 0) && (max(x) >= (u - sig_u / xi))

  if(condition_1 || condition_2){
    llh <- -10e6
    return((-1)^negative * llh)}

  # Log-likelihood
  prob <- dgpd(x = x, shape = xi, scale = sig_v, shift = v)
  llh <- sum(log(prob))

  return((-1)^negative * llh)

}
