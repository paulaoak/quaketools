% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/llh_gpd_mmax_mean_varu.R
\name{llh_gpd_mmax_mean_varu}
\alias{llh_gpd_mmax_mean_varu}
\title{Log-likelihood of GPD with variable threshold under the reparameterisation
of Mmax and mean.}
\usage{
llh_gpd_mmax_mean_varu(mmax_mean, u, v, x, negative = FALSE)
}
\arguments{
\item{mmax_mean}{GPD parameters mmax and mean at threshold u, that is the
the maximum (upper-end-point) and mean excess, respectively for the GPD.}

\item{u}{(Low) latent threshold of continuous GPD for which sigxi is given.}

\item{v}{Vector of latent threshold values for each observation.}

\item{x}{Vector of observed values.}

\item{negative}{Logical. Return negative log-likelihood?}
}
\value{
(Negative) log-likelihood of GPD data with variable threshold.
}
\description{
Function to evaluate the (negative) log-likelihood of GPD data
with variable threshold.
IMPORTANT: If you don't want a variable threshold just provide parameter v to
be v = u.
}
\examples{
#simulate data
v <- rep(c(1.6,1.2, 1.1), each = 50)
x <- rgpd_mmax_mean(n = 150,mmax = 10, mean = 5, shift = v)
llh_gpd_mmax_mean_varu(mmax_mean = c(11, 1), u = 1, v= v, x=x)
llh_gpd_mmax_mean_varu(mmax_mean = c(11, 5), u = 1, v= v, x=x)

}
