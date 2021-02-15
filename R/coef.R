#'
#' Return the parameter estimates of a given leaf of a SEM tree
#'
#' @export
#' 
coef.semtree <- function(object, ...)
{
  model <- object$model
  
  if (inherits(model, "MxModel") || inherits(model, "MxRAMModel")) {
  	
    return(omxGetParameters(model))
    
  } else if (inherits(model, "lavaan")) {
    
    return(lavaan::coef(model))
    
  } else {
    warning("Model class is not supported!")
    return(NULL)
  }
  
}