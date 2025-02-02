% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/llh_tgpd_rd_varu.R
\name{llh_tgpd_rd_varu}
\alias{llh_tgpd_rd_varu}
\title{Log-likelihood of rounded Tapered GPD with variable threshold.}
\usage{
llh_tgpd_rd_varu(sigxitheta, u, v, x, to_nearest = 0.1, negative = FALSE)
}
\arguments{
\item{sigxitheta}{Tapered GPD parameters at threshold u.}

\item{u}{(Low) latent threshold of continuous Tapered GPD for which sigxitheta is given.}

\item{v}{Vector of latent threshold values for each observation.}

\item{x}{Vector of observed (rounded) values.}

\item{to_nearest}{Level to which observed values are rounded.}

\item{negative}{Logical. Return negative log-likelihood?}
}
\value{
(Negative) log-likelihood of rounded Tapered GPD data with variable threshold.
}
\description{
Function to evaluate the (negative) log-likelihood of rounded
Tapered GPD data with variable threshold.
}
\examples{
#simulate data
v <- rep(c(1.6,1.2, 1.1), each = 50)
x <- rtgpd_rd(n = 150, scale = 1.1, shape = 0.2, shift = v, scale_taper = 2, to_nearest = 0.1)
llh_tgpd_rd_varu(sigxitheta = c(0.1, -0.01, 0.2), u = 1, v= v, x=x)
llh_tgpd_rd_varu(sigxitheta = c(0.1, 0.01, 0.2), u = 1, v= v, x=x)

}
