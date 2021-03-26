#'
#' Return the parameter estimates of a given leaf of a SEM tree
#' @param object semtree. A SEM tree node.
#' @param \dots Extra arguments. Currently unused.
#'
#'  @exportS3Method coef semtree
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