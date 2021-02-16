#' @exportS3Method 
summary.semtree <-
function(object, ...) {

  result <- list();
  
  if(inherits(object,"MxModel") || inherits(object,"MxRAMModel")){
    result$model.title <- object$model@name
  }
  if(inherits(object,"lavaan")){
    result$model.title <- "Model"
  }
  
  result$num.nodes <- getNumNodes(object)
  
  result$leaf.nodes <- nrow(getTerminalNodes(object))
  
  result$sample.size <- object$N
  
  result$parameter.names <- names(object$params)
  
  result$df <- object$df
  
  class(result) <- "summary.semtree";	
  return(result);
	
}