# M Matrix (Crowder)
# 
# Author: Francois Pelletier
#
# LGPL-3.0
###############################################################################


#' M Matrix (Crowder)
#' 
#' Identical to the V matrix by definition
#' @param Y Individual data sample
#' @param param Vector of parameters of the distribution function
#' @param variancef Variance function of the distribution
#' @param skewnessf Skewness function of the distribution
#' @param kurtosisf Kurtosis function of the distribution
#' @param dmean Derivative in respect to the parameter vector of the mean function of the distribution
#' @param dsd Derivative in respect to the parameter vector of the standard deviation function of the distribution 
#' @return M Matrix
#' @export M.Crowder
#' @author Francois Pelletier
M.Crowder <- function(param,Y,variancef,skewnessf,kurtosisf,dmean,dsd)
{
	((dmean(param) %o% dmean(param))+
				((skewnessf(param)*dmean(param)-2*dsd(param)) %o%
					(skewnessf(param)*dmean(param)-2*dsd(param)))/
				gammaf.Crowder(param,skewnessf,kurtosisf))/
			variancef(param)
}