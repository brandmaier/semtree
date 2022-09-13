minus2logLik_from_fitted_models <- function(x) {
  if (class(x) %in% c("OpenMx", "MxRAMModel")) {
    x$output$Minus2LogLikelihood
  } else if (class(x) == "ctsemFit") {
    x$mxobj$output$Minus2LogLikelihood
  } else if (class(x) ==  "lavaan") {
    -2*lavaan::logLik(x)
  } else {
    stop("minus2logLik_from_fitted_models() does not recognize the model")
  }
}