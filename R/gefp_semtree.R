gefp_semtree <- function (..., fit = NULL, scores, vcov = NULL,
                          decorrelate = TRUE, sandwich = TRUE, order.by = NULL,
                          fitArgs = NULL, parm = NULL, data = list()) {
  vcov. <- vcov
  fm <- list(...)
  fm <- fm[[1]]
  n <- NROW(scores)
  k <- NCOL(scores)
  z <- order.by
  order.name <- deparse(substitute(order.by))
  if (is.factor(z)) 
    z <- as.numeric(z)
  scores <- as.matrix(scores)
  if (inherits(z, "POSIXt")) 
    z <- suppressWarnings(c(z[1] + as.numeric(difftime(z[1], 
                                                       z[2], units = "secs")), z))
  else z <- suppressWarnings(c(z[1] - as.numeric(diff(z[1:2])), 
                               z))
  process <- scores/sqrt(n)
  if (is.null(vcov.)) {
    J <- crossprod(process)
    J12 <- strucchange::root.matrix(J)
  }
  else {
    if (sandwich) {
      Q <- chol2inv(chol(bread(fm)/n))
      J <- (Q %*% vcov.(fm, order.by = order.by, data = data) %*% 
              Q)/n
      J12 <- strucchange::root.matrix(J)
    }
    else {
      J12 <- vcov.
    }
  }
  process <- rbind(0, process)
  process <- apply(process, 2, cumsum)
  if (decorrelate) 
    process <- t(chol2inv(chol(J12)) %*% t(process))
  else {
    process <- t(1/sqrt(diag(J)) * t(process))
    if (length(parm) > 1) 
      stop("limiting process is not a Brownian bridge")
  }
  colnames(process) <- colnames(scores)
  if (!is.null(parm)) 
    process <- process[, parm]
  retval <- list(process = suppressWarnings(zoo::zoo(process, z)), 
                 nreg = k, nobs = n, call = match.call(), fit = fit, scores = scores, 
                 fitted.model = fm, par = NULL, lim.process = "Brownian bridge", 
                 type.name = "M-fluctuation test", order.name = order.name, 
                 J12 = J12)
  class(retval) <- "gefp"
  return(retval)
}
