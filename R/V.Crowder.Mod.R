# V Matrix (Modified Crowder)
# 
# Author: Francois Pelletier
#
# LGPL-3.0
###############################################################################


#' V Matrix (Modified Crowder)
#' @param Y Individual data sample
#' @param param Vector of parameters of the distribution function
#' @param variancef Variance function of the distribution
#' @param dmean Derivative in respect to the parameter vector of the mean function of the distribution
#' @param dsd Derivative in respect to the parameter vector of the standard deviation function of the distribution 
#' @return V Matrix
#' 
#' @author Francois Pelletier
V.Crowder.Mod <- function(param,Y,variancef,dmean,dsd)
{
	(variancef(param)*(a.Crowder.Mod(param,Y,variancef,dmean,dsd) %o% 
					a.Crowder.Mod(param,Y,variancef,dmean,dsd) + 
					sqrt(variancef(param)) * skewness(Y) * 
					(a.Crowder.Mod(param,Y,variancef,dmean,dsd) %o% 
						b.Crowder.Mod(param,Y,variancef,dmean,dsd) + 
						b.Crowder.Mod(param,Y,variancef,dmean,dsd) %o% 
						a.Crowder.Mod(param,Y,variancef,dmean,dsd)) + 
					variancef(param) * (kurtosis(Y)-3+2) * 
					b.Crowder.Mod(param,Y,variancef,dmean,dsd) %*% 
					t(b.Crowder.Mod(param,Y,variancef,dmean,dsd))))
}
