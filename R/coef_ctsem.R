# Quick and dirty function to get untramsformed parameter estimates from a
# ctsemFit object. This probably does not work for all tips of CTSEMs.

coef.ctsemFit <- function(x) {
  
  res <- x$mxobj$output$estimate
  
  if (any(c(x$mxobj$MANIFESTVARbase$free))) {
    values <- x$mxobj$MANIFESTVAR$result[x$mxobj$MANIFESTVARbase$free]
    labels <- x$mxobj$MANIFESTVARbase$labels[!is.na(x$mxobj$MANIFESTVARbase$labels)]
    res[labels] <- values
  }
  
  if (any(c(x$mxobj$DIFFUSIONbase$free))) {
    values <- x$mxobj$DIFFUSION$result[x$mxobj$DIFFUSIONbase$free]
    labels <- x$mxobj$DIFFUSIONbase$labels[!is.na(x$mxobj$DIFFUSIONbase$labels)]
    res[labels] <- values
  }
  
  if (any(c(x$mxobj$T0VARbase$free))) {
    values <- x$mxobj$T0VAR$result[x$mxobj$T0VARbase$free]
    labels <- x$mxobj$T0VARbase$labels[!is.na(x$mxobj$T0VARbase$labels)]
    res[labels] <- values
  }
  
  if (any(c(x$mxobj$TRAITVARbase$free))) {
    values <- x$mxobj$TRAITVAR$result[x$mxobj$TRAITVARbase$free]
    labels <- x$mxobj$TRAITVARbase$labels[!is.na(x$mxobj$TRAITVARbase$labels)]
    res[labels] <- values
  }
  
  res
  
}
