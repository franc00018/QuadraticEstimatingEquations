# M Matrix (gaussian)
# 
# Author: Francois Pelletier
#
# LGPL-3.0
###############################################################################

#' M Matrix (gaussian)
#' @param Y Individual data sample
#' @param param Vector of parameters of the distribution function
#' @param meanf Mean function of the distribution
#' @param variancef Variance function of the distribution
#' @param dmean Derivative in respect to the parameter vector of the mean function of the distribution
#' @param dsd Derivative in respect to the parameter vector of the standard deviation function of the distribution
#' @return M Matrix
#' 
#' @author Francois Pelletier
M.gauss <- function(param,Y,meanf,variancef,dmean,dsd)
{
	-(a.gauss(param,variancef,dmean) %o% dmean(param) + 
				2*sqrt(variancef(param)) * b.gauss(param,variancef,dsd) %*% t(dsd(param)))
}