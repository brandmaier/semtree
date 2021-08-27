
#' Predict terminal nodes
#' 
#' This function takes a dataset and predicts the terminal node id that
#' matches the given observation
#' 
#' @param object semtree. A SEM tree node.
#' @param data a data.frame with observations used for prediction
#' @param type String. What type of prediction should be made? Currently, this
#' only supports 'node_id', which returns the id of a terminal node.
#' @param \dots Optional arguments passed to the function.
#' @exportS3Method predict semtree
#' @author Caspar van Lissa, Andreas Brandmaier
#' 
predict.semtree <- function(object, data, type = "node_id", ...) {
  
  if(!hasArg(data)){
    if(inherits(object$model, "MxModel")){
      data <- object$model$data$observed
    } else {
      ui_stop("Argument 'data' required.")
    }
  }
  result <- switch(type,
                   "node_id" = { traverse(object, dataset = data)}
  )
  
  if (is.null(result)) ui_stop("Requested type no yet implemented in predict.semtree().")
  
  return(result)
} 
