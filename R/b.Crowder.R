# Second weighting vector of the quadratic estimating equation (Crowder)
# 
# Author: Francois Pelletier
#
# LGPL-3.0
###############################################################################

#' Second weighting vector of the quadratic estimating equation (Crowder)
#' 
#' @param param Vector of parameters of the distribution function
#' @param variancef Variance function of the distribution
#' @param skewnessf Skewness function of the distribution
#' @param kurtosisf Kurtosis function of the distribution
#' @param dmean Derivative in respect to the parameter vector of the mean function of the distribution
#' @param dsd Derivative in respect to the parameter vector of the standard deviation function of the distribution
#' @return First weighting vector
#' 
#' @author Francois Pelletier
b.Crowder <- function(param,variancef,skewnessf,kurtosisf,dmean,dsd)
{
	(skewnessf(param)*dmean(param)-
				2*dsd(param))/
			(variancef(param)^(3/2)*gammaf.Crowder(param,skewnessf,kurtosisf))	
}