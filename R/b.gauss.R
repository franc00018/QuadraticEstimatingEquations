# Second weighting vector of the quadratic estimating equation (gaussian)
# 
# Author: Francois Pelletier
#
# LGPL-3.0
###############################################################################

#' Second weighting vector of the quadratic estimating equation (gaussian)
#' @param param Vector of parameters of the distribution function
#' @param variancef Variance function of the distribution
#' @param dsd Derivative in respect to the parameter vector of the standard deviation function of the distribution
#' @return Second weighting vector
#' 
#' @author Francois Pelletier
b.gauss <- function(param,variancef,dsd)
{
	dsd(param)/variancef(param)
}


