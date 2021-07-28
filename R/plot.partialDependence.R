#' @exportS3Method plot partialDependence
plot.partialDependence <- function(x, type="l",xlab=NULL, ylab=NULL, ...)
{
  #if (!(x inherits ("partialDependence"))) {
  #  stop("Invalid x object not of class partialDependence");
  #}
  
  if (is.null(xlab)) {
    xlab <- x$reference.var
  }
  
  if (is.null(ylab)) {
    ylab <- x$reference.param
  }
  
  # collect
  col1 <- x$xgrid
  col2 <- rep(NA, length(col1))
  for (i in 1: length(col1)) {
    col2[i] <- x$dict[[as.character(col1[i])]]
    
  }
  
  if (x$is.factor) {
    barplot(col2, names.arg=col1,xlab = xlab,ylab = ylab, ...)
  } else {
    plot(col1, col2, type=type, xlab=xlab,ylab=ylab, ...)
  }
}
