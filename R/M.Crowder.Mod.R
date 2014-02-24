# M Matrix (Modified Crowder)
# 
# Author: Francois Pelletier
#
# LGPL-3.0
###############################################################################


#' M Matrix (Modified Crowder)
#' 
#' @param Y Individual data sample
#' @param param Vector of parameters of the distribution function
#' @param variancef Variance function of the distribution
#' @param skewnessf Skewness function of the distribution
#' @param kurtosisf Kurtosis function of the distribution
#' @param dmean Derivative in respect to the parameter vector of the mean function of the distribution
#' @param dsd Derivative in respect to the parameter vector of the standard deviation function of the distribution 
#' @return M Matrix
#' 
#' @author Francois Pelletier
M.Crowder.Mod <- function(param,Y,variancef,skewnessf,kurtosisf,dmean,dsd)
{
	-(a.Crowder.Mod(param,Y,variancef,dmean,dsd) %o% dmean(param) + 
				2*sqrt(variancef(param)) * 
				b.Crowder.Mod(param,Y,variancef,dmean,dsd) %*% t(dsd(param)))
}