# Wald test for estimating equations
# 
# Author: Francois Pelletier
#
# LGPL-3.0
###############################################################################


#' Wald test for estimating equations
#' @param param Estimated parameters
#' @param n Size of estimation sample
#' @param R Matrix of linear coefficients of the constraints
#' @param r vector of linear constants of the constraints
#' @param eqn.covariance Covariance matrix of the estimating equations
#' @param eqn.gradient Gradient matrix of the estimating equations
#' @param alpha level of confidence
#' @return A list containing the statistic, p-value and reject of the null hypothesis
#' @export Wald.Test
#' @author François Pelletier
Wald.Test <- function(param,n,R,r,eqn.covariance,eqn.gradient,alpha=0.05)
{
	wald.stat <- n * t(R %*% param - r) %*% 
			ginv(R %*% eqn.gradient %*% eqn.covariance %*% 
							t(eqn.gradient) %*% t(R)) %*% 
			(R %*% param - r)
	PV <- pchisq(wald.stat,nrow(R))
	list(wald.stat=wald.stat, p.value=PV, reject=PV>1-alpha)
}