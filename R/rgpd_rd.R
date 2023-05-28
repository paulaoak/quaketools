#' @inherit rgpd title description details
#'
#' @author Paula Cordero Encinar (based on zakvarty/threshold_paper_code)
#'
#' @inheritParams rgpd
#' @param to_nearest Precision of resulting observations.
#' @param shift_latent Latent shift parameter to account for rounding when sampling.
#'
#' @examples
#' rgpd_rd(n = 5, scale = 1, shape = 0, shift = 0)
#' rgpd_rd(n = 5, scale = 1:5, shape = 0.1, shift = 0, to_nearest = 0.1)
#' rgpd_rd(n = 5, scale = 1, shape = 0.1 * 1:5, to_nearest = 0.2)
#'
#' @export
rgpd_rd <- function(n, scale = 1, shape = 0, shift = 0, shape_tolerance = 1e-10, to_nearest = 1, shift_latent = NULL){
  # Check inputs
  stopifnot(exprs = {
    all(scale > 0)
    length(scale) >= 1
    length(shape) >= 1
    length(shift) >= 1
    length(to_nearest) == 1
    is.numeric(to_nearest)
  })

  # Function body
  if(is.null(shift_latent)) shift_latent = shift - 0.5 * to_nearest

  #sample gpd
  x <- rgpd(n = n, scale = scale, shape = shape, shift = shift_latent)
  #compute rounded observations
  y <- x/to_nearest
  y <- round(y)
  y <- y * to_nearest
  return(y)
}
