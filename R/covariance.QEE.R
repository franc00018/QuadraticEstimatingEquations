# Covariance matrix using the component matrix M and V from QEE
# 
# Author: Francois Pelletier
#
# LGPL-3.0
###############################################################################

#' Covariance matrix using the component matrix M and V from QEE
#'
#' Covariance matrix using the component matrix M and V from QEE
#' @param M Gradient matrix
#' @param V Covariance matrix of equations
#' @param n Sample size
#' @return Weighted covariance matrix
#' @author François Pelletier
covariance.QEE <- function(M,V,n) ## Omega
{
	ginv(M %*% ginv(V) %*% t(M))/n
}