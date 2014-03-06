# Second weighting vector of the modified quadratic estimating equation (Crowder)
# 
# Author: Francois Pelletier
#
# LGPL-3.0
###############################################################################

#' Second weighting vector of the modified quadratic estimating equation (Crowder)
#' 
#' @param param Vector of parameters of the distribution function
#' @param Y Individual data sample
#' @param variancef Variance function of the distribution
#' @param dmean Derivative in respect to the parameter vector of the mean function of the distribution
#' @param dsd Derivative in respect to the parameter vector of the standard deviation function of the distribution
#' @return First weighting vector
#' @export b.Crowder.Mod
#' @author Francois Pelletier
b.Crowder.Mod <- function(param,Y,variancef,dmean,dsd)
{
	(moments::skewness(Y)*dmean(param)-2*dsd(param)) /
			(variancef(param)^(3/2)*gammaf.Crowder.Mod(Y))
}