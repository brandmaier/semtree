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
  if (!hasArg(data)) {
    tryCatch({
      data <- object$data
    }, error = function(e) {
      stop("Argument 'data' required.")
    })
  }
  result <- switch(type,
                   node_id = {
                     f_stripped <- strip(object)
                     apply(data, 1, function(r) {
                       sapply(f_stripped,
                              traverse_stripped,
                              row = r,
                              what = "node_id")
                     })
                   },
                   pars = {
                     predict_pars(forest = object,
                                  data = data,
                                  ...)
                   })
  if (is.null(result))
    ui_stop("Requested type no yet implemented in predict.semtree().")
  class(result) <- c(paste0("semforest_", type), class(result))
  return(result)
}

#' @method predict semtree
#' @export
predict.semtree <- function(object, data, type = "node_id", ...) {
  if (!hasArg(data)) {
    if (inherits(object$model, "MxModel")) {
      data <- object$model$data$observed
    } else {
      stop("Argument 'data' required.")
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
  class(result) <- c(paste0("semforest_", type), class(result))
  return(result)
}

#' @method predict semforest_stripped
#' @export
predict.semforest_stripped <-
  function (object, data, type = "node_id", ...)
  {
    if (!hasArg(data)) {
      ui_stop("Argument 'data' required.")
    }
    result <- switch(type,
                     node_id = {
                       apply(data, 1, function(r) {
                         sapply(object,
                                traverse_stripped,
                                row = r,
                                what = "node_id")
                       })
                     },
                     pars = {
                       predict_pars(forest = object,
                                    data = data,
                                    ...)
                     })
    if (is.null(result))
      ui_stop("Requested type no yet implemented in predict.semtree().")
    class(result) <- c(paste0("semforest_", type), class(result))
    return(result)
  }

predict_pars <-
  function(forest,
           data,
           parameters = NULL,
           FUN = median,
           ...) {
    UseMethod("predict_pars", forest)
  }

predict_pars.semforest <-
  function(forest,
           data,
           parameters = NULL,
           FUN = median,
           ...) {
    cl <- match.call()
    cl[["forest"]] <- strip(forest)
    if (!hasArg(data))
      cl[["data"]] <- forest$data
    cl[[1L]] <- str2lang("semtree:::predict_pars")
    eval.parent(cl)
  }


predict_pars.semforest_stripped <-
  function(forest,
           data,
           parameters = NULL,
           FUN = median,
           ...) {
    if (!inherits(data, "data.table"))
      setDT(data)
    parnams <- attr(forest, "parameters")
    out <-
      data[, as.list(apply(do.call(cbind, lapply(forest, function(t) {
        traverse_stripped(row = .SD, tree = t)
      })), 1, FUN = FUN, ...)), by = 1:nrow(data)][,-1]
    
    
    setnames(out, names(out), attr(forest, "parameters"))
    if (!is.null(parameters))
      out <- out[, parameters, drop = FALSE]
    out
  }
