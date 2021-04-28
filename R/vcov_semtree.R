vcov_semtree <- function(x, ...) {
  UseMethod("vcov_semtree")
}

vcov_semtree.default <- function(x, ...) {
  vcov(x, ...)
}

vcov_semtree.lavaan <- function(x, ...) {
  if (x@Model@eq.constraints) {
    K <- eval(parse(text = "lavaan:::lav_constraints_R2K(x@Model)"))
    res <- solve(t(K) %*% lavaan::lavInspect(x, what = "information.expected") %*% K * 
                   nobs(x)) 
  } else {
    res <- x@vcov$vcov
  }
  res
}
