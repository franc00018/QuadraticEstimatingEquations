# Gamma function used in Crowder Estimating Equations
# 
# Author: Francois Pelletier
#
# LGPL-3.0
###############################################################################


#' Gamma function used in Crowder Estimating Equations
#' @param param Vector of parameters of the distribution function
#' @param skewnessf Skewness function of the distribution
#' @param kurtosisf Kurtosis function of the distribution
#' @return Gamma function value
#' 
#' @author Francois Pelletier
gammaf.Crowder <- function(param,skewnessf,kurtosisf)
{
	kurtosisf(param)+2-skewnessf(param)^2
}
