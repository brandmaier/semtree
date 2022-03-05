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

predict_pars <- function(forest, data, parameters = NULL, FUN = median, ...){
  UseMethod("predict_pars", forest)
}
predict_pars.semforest_light <- function(forest, data, parameters = NULL, FUN = median, ...){
  # Fix hier
  # some_fun <- function(a){
  #   c(is.character(a[["a"]]), is.numeric(a[["b"]]))
  # }
  # test[, all(some_fun(a = .SD)), by = 1:nrow(test)]
  ##r_list <- split(data, 1:nrow(data))
  parnams <- attr(forest, "parameters")
  out <- data[, as.list(apply(sapply(forest, function(t){ traverse_light(row = .SD, tree = t) }), 1, FUN = FUN, ...)), by = 1:nrow(data)][, -1]
  # out <- sapply(r_list, function(r){
  #   apply(sapply(forest, function(t){
  #     traverse_light(r, t)
  #   }), 1, FUN = FUN)
  # })
  
  # out <- sapply(data, 1, function(r){
  #   apply(
  #     sapply(forest, function(t){
  #       traverse_light(r, t)
  #     })      
  #     , 1, FUN = FUN)
  # })
  # out <- t(out)
  # colnames(out) <- attr(forest, "parameters")
  setnames(out, names(out), attr(forest, "parameters"))
  if(!is.null(parameters)) out <- out[, parameters, drop = FALSE]
  out
}
