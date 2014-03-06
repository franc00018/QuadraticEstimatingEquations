# Quadratic estimating equation (Crowder)
# 
# Author: Francois Pelletier
#
# LGPL-3.0
###############################################################################


#' Quadratic estimating equation (Crowder)
#' @param Y Individual data sample
#' @param param Vector of parameters of the distribution function
#' @param meanf Mean function of the distribution
#' @param variancef Variance function of the distribution
#' @param skewnessf Skewness function of the distribution
#' @param kurtosisf Kurtosis function of the distribution
#' @param dmean Derivative in respect to the parameter vector of the mean function of the distribution
#' @param dsd Derivative in respect to the parameter vector of the standard deviation function of the distribution
#' @return The vector value of the estimating equation
#' @export eqn.Crowder
#' @author Francois Pelletier
eqn.Crowder <- function(param,Y,meanf,variancef,skewnessf,kurtosisf,dmean,dsd)
{
	a.Crowder(param,variancef,skewnessf,kurtosisf,dmean,dsd) * sum(Y-meanf(param)) +
			b.Crowder(param,variancef,skewnessf,kurtosisf,dmean,dsd) * sum((Y-meanf(param))^2-variancef(param))
}
