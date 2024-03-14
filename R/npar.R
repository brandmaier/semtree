npar <- function(x) {
  if (is(x,"OpenMx") || is(x,"MxRAMModel")) {
    stop("npar() not implemented yet")
  } else if (is(x, "ctsemFit")) {
    stop("npar() not implemented yet")
  } else if (is(x,"lavaan")) {
    pt <- parameterTable(x)
    sum(pt$free!=0)
  } else {
    stop("npar() not implemented yet")
  }
}