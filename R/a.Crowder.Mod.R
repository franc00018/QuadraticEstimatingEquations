# First weighting vector of the modified quadratic estimating equation (Crowder)
# 
# Author: Francois Pelletier
#
# LGPL-3.0
###############################################################################

#' First weighting vector of the modified quadratic estimating equation (Crowder)
#' 
#' @param param Vector of parameters of the distribution function
#' @param Y Individual data sample
#' @param variancef Variance function of the distribution
#' @param dmean Derivative in respect to the parameter vector of the mean function of the distribution
#' @param dsd Derivative in respect to the parameter vector of the standard deviation function of the distribution
#' @return First weighting vector
#' 
#' @author Francois Pelletier
a.Crowder.Mod <- function(param,Y,variancef,dmean,dsd)
{
	(-(moments::kurtosis(Y)-1)*dmean(param)+
				2*moments::skewness(Y)*dsd(param))/
			(variancef(param)*gammaf.Crowder.Mod(Y))
}