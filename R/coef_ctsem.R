# Quick and dirty function to get untransformed parameter estimates from a
# ctsemFit object. This probably does not work for all tips of CTSEMs.

coef.ctsemFit <- function(object, ...) {
  
  res <- object$mxobj$output$estimate
  
  if (any(c(object$mxobj$MANIFESTVARbase$free))) {
    values <- object$mxobj$MANIFESTVAR$result[object$mxobj$MANIFESTVARbase$free]
    labels <- object$mxobj$MANIFESTVARbase$labels[!is.na(object$mxobj$MANIFESTVARbase$labels)]
    res[labels] <- values
  }
  
  if (any(c(object$mxobj$DIFFUSIONbase$free))) {
    values <- object$mxobj$DIFFUSION$result[object$mxobj$DIFFUSIONbase$free]
    labels <- object$mxobj$DIFFUSIONbase$labels[!is.na(object$mxobj$DIFFUSIONbase$labels)]
    res[labels] <- values
  }
  
  if (any(c(object$mxobj$T0VARbase$free))) {
    values <- object$mxobj$T0VAR$result[object$mxobj$T0VARbase$free]
    labels <- object$mxobj$T0VARbase$labels[!is.na(object$mxobj$T0VARbase$labels)]
    res[labels] <- values
  }
  
  if (any(c(object$mxobj$TRAITVARbase$free))) {
    values <- object$mxobj$TRAITVAR$result[object$mxobj$TRAITVARbase$free]
    labels <- object$mxobj$TRAITVARbase$labels[!is.na(object$mxobj$TRAITVARbase$labels)]
    res[labels] <- values
  }
  
  res
  
}
