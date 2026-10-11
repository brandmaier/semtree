#' Hellinger distance of two multivariate normal distributions
#'
#' @description
#' Compute distance between two multivariate normal distribution
#' of matching dimension. To be used with the diversity function.
#' 
#' 
#' @param mu1 Mean vector of the first distribution
#' @param cov1 Covariance matrix of the first distribution
#' @param mu2 Mean vector of the second distribution
#' @param cov2 Covariance matrix of the second distribution
#
#'
#' @export
hellinger <- function(mu1, cov1, mu2, cov2) {

  d <- nrow(cov1)
  stopifnot(d==nrow(cov2))

  mudiff <- mu2 - mu1
  
  inner <- 0.5* cov1 + 0.5*cov2
  
  a <- (det(cov1)^0.25*det(cov2)*0.25/sqrt(det(inner)))
  b <- exp( -.125*t(mudiff)%*%inner^-1%*%mudiff)
  
  diff <- 1-(
    a*b
  )
  
  
  return(diff)
}