# returns a Boolean to indicate whether the model has converged
hasConverged <- function(model)
{
  if (is.null(model)) return(NA)
  
  if (inherits(model, "MxModel") || inherits(model, "MxRAMModel")) {
    
    if (is.null(model$output) ||
      is.null(model$output$status) ||
      is.null(model$output$status$code)) {
      browser()
        warning("Could not check convergence!")
        return(NA)
      }
    
    return(model$output$status$code == 0)
    
  } else if (inherits(model, "lavaan")) {
    return(lavaan::lavInspect(model,"converged"))
  } else {
    stop("hasConverged() is not implemented for this model class! You can deactivate convergence checks in the semtree_control.")
  }
}