
# This function was contributed by Yves Rosseel and later modified
# assumes a multivariate normal model without any fancy modifications
# assumes complete data
lavaan_casewise_loglik_matrices <- function(Y, mu = NULL, Sigma = NULL) {

  Y <- as.matrix(Y)
  if (ncol(Y) == 1L && nrow(Y) == nrow(Sigma)) {
    # just a vector
    Y <- t(Y)
  }
  
  if (is.null(mu)) { 
    mu <- colMeans(Y)
  }
  
  P <- ncol(Sigma)
  LOG.2PI <- log(2 * pi)
  cS <- chol(Sigma)
  icS <- backsolve(cS, diag(P))
  

  mu <- drop(mu)
  Yc <- t(t(Y) - mu)
  DIST <- rowSums((Yc %*% icS)^2)

  logdet <- -2 * sum(log(diag(icS)))
  loglik <- -(P * LOG.2PI + logdet + DIST)/2
  loglik
}

