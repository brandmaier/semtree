getData <- function(x, ...) {
  UseMethod("getData")
}

getData.tree <- function(x, ...) {
  if (!inherits(x, "tree")) {
    stop("x must inherit from class 'tree'")
  }
  
  if (is.null(x$data)) {
    stop("tree object does not contain a 'data' component")
  }
  
  # additional checks here
  x$data
}