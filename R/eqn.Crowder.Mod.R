# Modified Quadratic estimating equation (Crowder)
# 
# Author: Francois Pelletier
#
# LGPL-3.0
###############################################################################


#'  Modified Quadratic estimating equation (Crowder)
#' @param Y Individual data sample
#' @param param Vector of parameters of the distribution function
#' @param meanf Mean function of the distribution
#' @param variancef Variance function of the distribution
#' @param dmean Derivative in respect to the parameter vector of the mean function of the distribution
#' @param dsd Derivative in respect to the parameter vector of the standard deviation function of the distribution
#' @return The vector value of the estimating equation
#' @export eqn.Crowder.Mod
#' @author Francois Pelletier
eqn.Crowder.Mod <- function(param,Y,meanf,variancef,dmean,dsd)
{
	a.Crowder.Mod(param,Y,variancef,dmean,dsd) * sum(Y-meanf(param)) +
			b.Crowder.Mod(param,Y,variancef,dmean,dsd) * sum((Y-meanf(param))^2-variancef(param))
}
