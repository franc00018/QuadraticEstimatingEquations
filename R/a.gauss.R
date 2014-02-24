# First weighting vector of the quadratic estimating equation (gaussian)
# 
# Author: Francois Pelletier
#
# LGPL-3.0
###############################################################################

#' First weighting vector of the quadratic estimating equation (gaussian)
#' @param param Vector of parameters of the distribution function
#' @param variancef Variance function of the distribution
#' @param dmean Derivative in respect to the parameter vector of the mean function of the distribution
#' @return First weighting vector
#' 
#' @author Francois Pelletier
a.gauss <- function(param,variancef,dmean)
{
	dmean(param)/variancef(param)
}