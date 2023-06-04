#' @title Log-likelihood of rounded GPD with variable threshold.
#'
#' @description Function to evaluate the (negative) log-likelihood of rounded GPD data with variable threshold.
#'
#' @param sigxi GPD parameters at threshold u.
#' @param u (Low) latent threshold of continuous GPD for which sigxi is given.
#' @param v Vector of latent threshold values for each observation.
#' @param x Vector of observed (rounded) values.
#' @param to_nearest Level to which observed values are rounded.
#' @param negative Logical. Return negative log-likelihood?
#'
#' @return (Negative) log-likelihood of rounded GPD data with variable threshold.
#'
#' @examples
#' #simulate data
#' v <- rep(c(1.6,1.2, 1.1), each = 50)
#' x <- rgpd_rd(n = 150,scale = 1.1, shape = 0.2, shift = v, to_nearest = 0.1)
#' llh_gpd_rd_varu(sigxi = c(0.1, -0.01), u = 1, v= v, x=x)
#' llh_gpd_rd_varu(sigxi = c(0.1, -0.5), u = 1, v= v, x=x)
#'
#' @export
llh_gpd_rd_varu <- function(sigxi, u, v, x, to_nearest = 0.1, negative = FALSE){

  # Check inputs
  stopifnot(exprs = {
    is.numeric(u)
    length(u) == 1
    is.numeric(sigxi)
    length(sigxi)==2 #unique shape and scale_u parameters
    is.numeric(to_nearest)
    length(to_nearest) == 1
    is.logical(negative)
    u <= min(v)
  })

  # Latent GPD parameters
  sig_u <- sigxi[1]
  xi <- sigxi[2]
  delta <- to_nearest/2
  sig_v <- sig_u + xi* (v-u) #threshold dependent scale parameter


  # Check x all be above v (adjusted for rounding)
  lep_fail <- any(x < v - delta)
  if(lep_fail){stop('Lower endpoint failure:  !all(x > v - to_nearest / 2).')}


  # Function body

  # Satisfy conditions to be a valid rounded GPD distribution:
  #check all scale parameters are positive
  condition_1 <- sig_u <= 0
  #check all x below upper end-point (UEP) (if it exists, adjusting for rounding)
  condition_2 <- (xi < 0) && (max(x) >= (u - sig_u / xi + delta))

  if(condition_1 || condition_2){
    llh <- -10e6
    return((-1)^negative * llh)}

  # Log-likelihood based on equation (2) of Z.Varty et al. 2021
  #points to evaluate distribution function
  x_high <- x + delta
  x_bottom <- x-delta
  x_max <- pmax(x_bottom, v)

  p_high <- pgpd(q = x_high, shape = xi, scale = sig_v, shift = v)
  p_max <- pgpd(q = x_max,  shape = xi, scale = sig_v, shift = v)

  #obtain weights
  weight <- (pgpd(q = x_high,  shape = xi, scale = sig_u, shift = u) -
               pgpd(q = x_max,  shape = xi, scale = sig_u, shift = u)) /
    (pgpd(q = x_high,  shape = xi, scale = sig_u, shift = u) -
       pgpd(q = x_bottom,  shape = xi, scale = sig_u, shift = u))

  llh <- sum(weight * (log(p_high - p_max)))
  return((-1)^negative * llh)

}
