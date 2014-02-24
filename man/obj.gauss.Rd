\name{obj.gauss}
\alias{obj.gauss}
\title{Quadratic form objective function for optimization of the parameter vector (gaussian)}
\usage{
obj.gauss(param, Y, meanf, variancef, dmean, dsd, Q = diag(4))
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

  \item{Q}{Weight matrix}
}
\value{
The value of the quadratic form
}
\description{
Quadratic form objective function for optimization of the
parameter vector (gaussian)
}
\author{
Francois Pelletier
}

