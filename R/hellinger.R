
hellinger <- function(mu1, cov1, mu2, cov2) {

  d <- nrow(cov1)

  mudiff <- mu2 - mu1
  
  inner <- 0.5* cov1 + 0.5*cov2
  
  a <- (det(cov1)^0.25*det(cov2)*0.25/sqrt(det(inner)))
  b <- exp( -.125*t(mudiff)%*%inner^-1%*%mudiff)
  
  diff <- 1-(
    a*b
  )
  
  
  return(diff)
}