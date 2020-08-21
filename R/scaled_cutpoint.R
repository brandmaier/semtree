scaled_cutpoint <- function(CSP, covariate, from, to) {
  x <- apply(X = CSP, MARGIN = 1, FUN = function(x) {sum(x^2)})
  n <- length(x)
  n1 <- floor(from * n)
  n2 <- floor(to * n)
  tt <- seq_along(x)/n
  x <- x[n1:n2]
  tt <- tt[n1:n2]
  x <- x/(tt * (1 - tt))
  max.cov <- covariate[which.max(x) + n1 - 1]
return((max.cov + covariate[which(covariate > max.cov)[1]]) / 2)
}
