minus2logLik_from_fitted_models <- function(x) {
  if (is(x,"OpenMx") || is(x,"MxRAMModel")) {
    x$output$Minus2LogLikelihood
  } else if (is(x, "ctsemFit")) {
    x$mxobj$output$Minus2LogLikelihood
  } else if (is(x,"lavaan")) {
    -2*lavaan::logLik(x)
  } else {
    stop("minus2logLik_from_fitted_models() does not recognize the model")
  }
}