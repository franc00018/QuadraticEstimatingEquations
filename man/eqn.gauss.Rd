\name{eqn.gauss}
\alias{eqn.gauss}
\title{Quadratic estimating equation (gaussian)}
\usage{
eqn.gauss(param, Y, meanf, variancef, dmean, dsd)
}
\arguments{
  \item{Y}{Individual data sample}

  \item{param}{Vector of parameters of the distribution
  function}

  \item{meanf}{Mean function of the distribution}

  \item{variancef}{Variance function of the distribution}

  \item{dmean}{Derivative in respect to the parameter
  vector of the mean function of the distribution}

  \item{dsd}{Derivative in respect to the parameter vector
  of the standard deviation function of the distribution}
}
\value{
The vector value of the estimating equation
}
\description{
Quadratic estimating equation (gaussian)
}
\author{
Francois Pelletier
}

