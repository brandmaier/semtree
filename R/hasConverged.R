# returns a Boolean to indicate whether the model has converged
hasConverged <- function(model)
{
  if (inherits(model, "MxModel") || inherits(model, "MxRAMModel")) {
    
    return(model$output$status$code == 0)
    
  } else if (inherits(model, "lavaan")) {
    return(lavaan::lavInspect(model,"converged"))
  } else {
    stop("hasConverged() is not implemented for this model class!")
  }
}