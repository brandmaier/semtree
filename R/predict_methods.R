#' Predict method for semtree and semforest
#' 
#' @param object Object of class \code{semtree} or `semforest`.
#' @param data New test data of class \code{data.frame}. If no data is provided,
#' attempts to extract the data from the object.
#' @param type Type of prediction. One of `c('node_id')`. See Details.
#' @param ... further arguments passed to or from other methods.
#' @return Object of class \code{matrix}.
#' @author Caspar J. van Lissa
#' @method predict semforest
#' @export
predict.semforest <- function(object, data, type = "node_id", ...) {
  cl <- match.call()
  cl[[1L]] <- quote(predict)
  if(!hasArg(data)){
    tryCatch({cl[["data"]] <- object$data}, error = function(e){
      stop("Argument 'data' required.")
    })
  }
  result <- future.apply::future_sapply(X = object$forest, FUN = function(t){
    cl[["object"]] <- t
    eval.parent(cl)
  })
  return(result)
}

#' @method predict semtree
#' @export
predict.semtree <- function(object, data, type = "node_id", ...) {
  if(!hasArg(data)){
    if(inherits(object$model, "MxModel")){
      data <- object$model$data$observed
    } else {
      stop("Argument 'data' required.")
    }
  }
  result <- switch(type,
                   "node_id" = { traverse(object, dataset = data)}
                   )
  return(result)
}