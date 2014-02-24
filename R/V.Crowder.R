# V Matrix (Crowder)
# 
# Author: Francois Pelletier
#
# LGPL-3.0
###############################################################################


#' V Matrix (Crowder)
#' @param Y Individual data sample
#' @param param Vector of parameters of the distribution function
#' @param variancef Variance function of the distribution
#' @param skewnessf Skewness function of the distribution
#' @param kurtosisf Kurtosis function of the distribution
#' @param dmean Derivative in respect to the parameter vector of the mean function of the distribution
#' @param dsd Derivative in respect to the parameter vector of the standard deviation function of the distribution 
#' @return V Matrix
#' 
#' @author Francois Pelletier
V.Crowder <- function(param,Y,variancef,skewnessf,kurtosisf,dmean,dsd)
{
	((dmean(param) %o% dmean(param))+
				((skewnessf(param)*dmean(param)-2*dsd(param)) %o%
					(skewnessf(param)*dmean(param)-2*dsd(param)))/
				gammaf.Crowder(param,skewnessf,kurtosisf))/
			variancef(param)
}
