#' @importFrom grDevices heat.colors
colmap <- function(x, cmap=heat.colors) {
  xvals <- unique(x)
  cols <- cmap(length(xvals))
  return(cols[unclass(as.factor(x))])
}
