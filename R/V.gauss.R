# V Matrix (gaussian)
# 
# Author: Francois Pelletier
#
# LGPL-3.0
###############################################################################


#' V Matrix (gaussian)
#' @param Y Individual data sample
#' @param param Vector of parameters of the distribution function
#' @param meanf Mean function of the distribution
#' @param variancef Variance function of the distribution
#' @param skewnessf Skewness function of the distribution
#' @param kurtosisf Kurtosis function of the distribution
#' @param dmean Derivative in respect to the parameter vector of the mean function of the distribution
#' @param dsd Derivative in respect to the parameter vector of the standard deviation function of the distribution 
#' @return V Matrix
#' 
#' @author Francois Pelletier
V.gauss <- function(param,Y,meanf,variancef,skewnessf,kurtosisf,dmean,dsd)
{
	(variancef(param)*(a.gauss(param,variancef,dmean,dsd) %o% 
					a.gauss(param,variancef,dmean,dsd) + 
					sqrt(variancef(param)) * skewnessf(param) * 
					(a.gauss(param,variancef,dmean,dsd) %o% 
						b.gauss(param,variancef,dmean,dsd) +
						b.gauss(param,variancef,dmean,dsd) %o% 
						a.gauss(param,variancef,dmean,dsd)) + 
					variancef(param)*(kurtosis(param)+2) * 
					b.gauss(param,variancef,dmean,dsd) %*% 
					t(b.gauss(param,variancef,dmean,dsd))))
}
