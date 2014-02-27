\name{Wald.Test}
\alias{Wald.Test}
\title{Wald test for estimating equations}
\usage{
Wald.Test(param, n, R, r, eqn.covariance, eqn.gradient, alpha = 0.05)
}
\arguments{
  \item{param}{Estimated parameters}

  \item{n}{Size of estimation sample}

  \item{R}{Matrix of linear coefficients of the
  constraints}

  \item{r}{vector of linear constants of the constraints}

  \item{eqn.covariance}{Covariance matrix of the estimating
  equations}

  \item{eqn.gradient}{Gradient matrix of the estimating
  equations}

  \item{alpha}{level of confidence}
}
\value{
A list containing the statistic, p-value and reject of the
null hypothesis
}
\description{
Wald test for estimating equations
}
\author{
François Pelletier
}

