getParameterNames <- function(x)
{
  if (inherits(x,"MxModel") || inherits(x,"MxRAMModel")) {
    return(names(OpenMx::omxGetParameters(x)))
  } else  if (inherits(x,"lavaan")){
    return(unique(partable(x)$label))
  } else {
   stop("getParameterNames() is not implemented!")
  }
}