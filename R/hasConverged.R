hasConverged <- function(model)
{
  if (inherits(model, "MxModel") || inherits(model, "MxRAMModel")) {
    
    return(model$output$status$code == 0)
    
  } else if (inherits(model, "lavaan")) {
    stop("hasConverged() is not implemented for this model class!")    
  } else {
    stop("hasConverged() is not implemented for this model class!")
  }
}