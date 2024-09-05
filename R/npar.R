npar <- function(x) {
  if (is(x, "OpenMx") || is(x, "MxRAMModel")) {
    length(OpenMx::omxGetParameters(x))
  } else if (is(x, "ctsemFit")) {
    stop("npar() not implemented yet for ctsemFit")
  } else if (is(x, "lavaan")) {
    pt <- lavaan::parameterTable(x)
    sum(pt$free != 0)
  } else {
    stop("npar() not implemented yet")
  }
}
