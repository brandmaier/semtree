predict.semforest_light <- function (object, data, type = "node_id", ...) 
{
  if (!hasArg(data)) {
    if (inherits(object$model, "MxModel")) {
      data <- object$model$data$observed
    }
    else {
      ui_stop("Argument 'data' required.")
    }
  }
  result <- switch(type, 
                   node_id = {
                     traverse(object, dataset = data)
                     },
                   pars = {
                     predict_pars(forest = object,
                                  data = data,
                                  ...)
                   })
  if (is.null(result)) 
    ui_stop("Requested type no yet implemented in predict.semtree().")
  return(result)
}

predict_pars <- function(forest, data, parameters = NULL, fun = median, ...){
  UseMethod("predict_pars", forest)
}
predict_pars.semforest_light <- function(forest, data, parameters = NULL, FUN = median, ...){
  out <- t(apply(data, 1, function(r){
    apply(
      sapply(forest, function(t){
        traverse_light(r, t)
      })      
      , 1, FUN = FUN)
  }))
  colnames(out) <- attr(forest, "parameters")
  if(!is.null(parameters)) out <- out[, parameters, drop = FALSE]
  out
}
