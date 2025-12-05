getParameterNames <- function(x)
{
  if (inherits(model,"MxModel") || inherits(model,"MxRAMModel")) {
    return(names(OpenMx::omxGetParameters(x)))
  } else  if (inherits(model,"lavaan")){
    return(unique(partable(x)$label))
  } else {
   stop("getParameterNames() is not implemented!")
  }
}