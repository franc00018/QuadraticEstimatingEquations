# Quadratic estimating equation (gaussian)
# 
# Author: Francois Pelletier
#
# LGPL-3.0
###############################################################################


#' Quadratic estimating equation (gaussian)
#' @param Y Individual data sample
#' @param param Vector of parameters of the distribution function
#' @param meanf Mean function of the distribution
#' @param variancef Variance function of the distribution
#' @param dmean Derivative in respect to the parameter vector of the mean function of the distribution
#' @param dsd Derivative in respect to the parameter vector of the standard deviation function of the distribution
#' @return The vector value of the estimating equation
#' 
#' @author Francois Pelletier
eqn.gauss <- function(param,Y,meanf,variancef,dmean,dsd)
{
	a.gauss(param,variancef,dmean) * sum(Y-meanf(param)) +
			b.gauss(param,variancef,dsd) * sum((Y-meanf(param))^2-variancef(param))
}
