#' @title Log-likelihood of rounded Tapered GPD with variable threshold.
#'
#' @description Function to evaluate the (negative) log-likelihood of rounded
#' Tapered GPD data with variable threshold.
#'
#' @param sigxitheta Tapered GPD parameters at threshold u.
#' @param u (Low) latent threshold of continuous Tapered GPD for which sigxitheta is given.
#' @param v Vector of latent threshold values for each observation.
#' @param x Vector of observed (rounded) values.
#' @param to_nearest Level to which observed values are rounded.
#' @param negative Logical. Return negative log-likelihood?
#'
#' @return (Negative) log-likelihood of rounded Tapered GPD data with variable threshold.
#'
#' @examples
#' #simulate data
#' v <- rep(c(1.6,1.2, 1.1), each = 50)
#' x <- rtgpd_rd(n = 150, scale = 1.1, shape = 0.2, shift = v, scale_taper = 2, to_nearest = 0.1)
#' llh_tgpd_rd_varu(sigxitheta = c(0.1, -0.01, 0.2), u = 1, v= v, x=x)
#' llh_tgpd_rd_varu(sigxitheta = c(0.1, 0.01, 0.2), u = 1, v= v, x=x)
#'
#' @export
llh_tgpd_rd_varu <- function(sigxitheta, u, v, x, to_nearest = 0.1, negative = FALSE){

  # Check inputs
  stopifnot(exprs = {
    is.numeric(u)
    length(u) == 1
    is.numeric(sigxitheta)
    length(sigxitheta)==3 #unique shape, scale_u, scale_theta parameters
    is.numeric(to_nearest)
    length(to_nearest) == 1
    is.logical(negative)
    u <= min(v)
  })

  # Latent tapered GPD parameters
  sig_u <- sigxitheta[1]
  xi <- sigxitheta[2]
  theta <- sigxitheta[3]
  delta <- to_nearest/2
  sig_v <- sig_u + xi* (v-u) #threshold dependent scale parameter


  # Check x all be above v (adjusted for rounding)
  lep_fail <- any(x < v - delta)
  if(lep_fail){stop('Lower endpoint failure:  !all(x > v - to_nearest / 2).')}


  # Function body

  # Satisfy conditions to be a valid rounded tapered GPD distribution:
  #check all scale parameters are positive
  condition_1 <- sig_u <= 0
  condition_2 <- theta <= 0
  #check all x below upper end-point (UEP) (if it exists, adjusting for rounding)
  condition_3 <- (xi < 0) && (max(x) >= round((u - sig_u / xi)/to_nearest)*to_nearest)

  if(condition_1 || condition_2|| condition_3){
    llh <- -10e6
    return((-1)^negative * llh)}

  # Log-likelihood based on equation (2) of Z.Varty et al. 2021 modifyin the distribution function
  #points to evaluate distribution function
  x_high <- x + delta
  x_bottom <- x-delta
  x_max <- pmax(x_bottom, v)

  p_high <- ptgpd(q = x_high, shape = xi, scale = sig_v, shift = v, scale_taper = theta)
  p_max <- ptgpd(q = x_max,  shape = xi, scale = sig_v, shift = v, scale_taper = theta)

  #obtain weights
  weight <- (ptgpd(q = x_high,  shape = xi, scale = sig_u, shift = u, scale_taper = theta) -
               ptgpd(q = x_max,  shape = xi, scale = sig_u, shift = u, scale_taper = theta)) /
    (ptgpd(q = x_high,  shape = xi, scale = sig_u, shift = u, scale_taper = theta) -
       ptgpd(q = x_bottom,  shape = xi, scale = sig_u, shift = u, scale_taper = theta))

  #make sure that points for which x_max = x_bottom have weight 1
  weight[x_bottom == x_max] <- 1

  llh <- sum(weight * (log(p_high - p_max)))
  return((-1)^negative * llh)

}
