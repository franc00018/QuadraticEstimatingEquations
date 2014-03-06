# Confidence interval for QEE estimates
# 
# Author: Francois Pelletier
#
# LGPL-3.0
###############################################################################

#' Confidence interval for QEE estimates
#'
#' Uses covariance.GEE
#' @param param Vector of parameters of the distribution function
#' @param covariance Covariance matrix
#' @param alpha confidence level
#' @return 3 line matrix with lower bound, estimate and upper bound
#' @export confidence.interval.QEE
#' @author François Pelletier
confidence.interval.QEE <- function(param,covariance,n,alpha=0.05)
{
	cbind(LOWER=param - sqrt(diag(covariance))*qt(alpha,n,lower=F),
			ESTIMATE=param,
			UPPER=param + sqrt(diag(covariance))*qt(alpha,n,lower=F))
}