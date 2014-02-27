\name{confidence.interval.QEE}
\alias{confidence.interval.QEE}
\title{Confidence interval for QEE estimates}
\usage{
confidence.interval.QEE(param, covariance, n, alpha = 0.05)
}
\arguments{
  \item{param}{Vector of parameters of the distribution
  function}

  \item{covariance}{Covariance matrix}

  \item{alpha}{confidence level}
}
\value{
3 line matrix with lower bound, estimate and upper bound
}
\description{
Uses covariance.GEE
}
\author{
François Pelletier
}

