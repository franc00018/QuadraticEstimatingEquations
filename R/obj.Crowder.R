# Quadratic form objective function for optimization of the parameter vector (Crowder)
# 
# Author: Francois Pelletier
#
# LGPL-3.0
###############################################################################

#' Quadratic form objective function for optimization of the parameter vector (Crowder)
#' @param Y Individual data sample
#' @param param Vector of parameters of the distribution function
#' @param meanf Mean function of the distribution
#' @param variancef Variance function of the distribution
#' @param skewnessf Skewness function of the distribution
#' @param kurtosisf Kurtosis function of the distribution
#' @param dmean Derivative in respect to the parameter vector of the mean function of the distribution
#' @param dsd Derivative in respect to the parameter vector of the standard deviation function of the distribution
#' @param Q Weight matrix
#' @return The value of the quadratic form
#' @export obj.Crowder
#' @author Francois Pelletier
obj.Crowder <- function(param,Y,meanf,variancef,skewnessf,kurtosisf,dmean,dsd,Q=diag(4))
{
	eqn.Crowder(param,Y,meanf,variancef,skewnessf,kurtosisf,dmean,dsd) %*% Q %*% 
			eqn.Crowder(param,Y,meanf,variancef,skewnessf,kurtosisf,dmean,dsd)
}


