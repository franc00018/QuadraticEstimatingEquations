# Gamma function used in Modified Crowder Estimating Equations
# 
# Author: Francois Pelletier
#
# LGPL-3.0
###############################################################################


#' Gamma function used in Modified Crowder Estimating Equations
#' @param Y Individual data sample
#' @return Gamma function value
#' 
#' @author Francois Pelletier
gammaf.Crowder.Mod <- function(Y)
{
	moments::kurtosis(Y)-1-moments::skewness(Y)^2
}
