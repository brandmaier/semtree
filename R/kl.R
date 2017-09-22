
klsym <- function(mu1, cov1, mu2, cov2) {
  return( kl(mu1, cov1, mu2, cov2)+kl(mu2, cov2,mu1, cov1))
}

# compute similarity between two models
kl <- function(mu1, cov1, mu2, cov2) {
  
  d <- nrow(cov1)
  
  mudiff <- mu2 - mu1
  
  icov2 <- cov2^-1
  
  dist <- log(det(cov2))-log(det(cov1))+tr(icov2 %*% cov1)-d+
    t(mudiff) %*% icov2 %*% mudiff
  
  return(dist)
}
