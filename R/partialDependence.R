#' Compute partial dependence
#' 
#' Compute the partial dependence of a predictor, or set of predictors,
#' on a model parameter.
#' 
#' @param x An object for which a method exists
#' @param data Optional \code{data.frame} that was used to train the
#' model.
#' @param reference.var Character vector, referring to the (independent)
#' reference variable or variables for which partial dependence is calculated.
#' Providing two (or more) variables allows for probing interactions, but note
#' that this is computationally expensive.
#' @param support Integer. Number of grid points for interpolating the
#' \code{reference.var}. Alternatively, use \code{points} for one or more
#' variables named in \code{reference.var}.
#' @param points Named list, with elements corresponding to \code{reference.var}
#' . Use this argument to provide specific points for which to obtain marginal
#' dependence values; for example, the mean and +/- 1SD of \code{reference.var}.
#' @param mc Integer. If \code{mc} is not \code{NULL}, the function will sample
#' \code{mc} number of rows from \code{data} with replacement, to estimate 
#' marginal dependency using Monte Carlo integration. This is less
#' computationally expensive.
#' @param FUN Character string with function used to integrate predictions
#' across all elements of \code{x}.
#' @param ... Extra arguments passed to \code{FUN}.
#' @author Caspar J. Van Lissa, , Andreas M. Brandmaier
#' @export
partialDependence <- function(x, data, reference.var, support = 20, points = NULL, mc = NULL, FUN = "median", ...) {
  UseMethod("partialDependence", x)
}

#' @method partialDependence semforest
#' @importFrom methods hasArg
#' @export
partialDependence.semforest <- function(x, data, reference.var, support = 20, points = NULL, mc = NULL, FUN = "median", ...){
  if (!all(reference.var %in% x$covariates)) {
    ui_stop("The following predictors are not in the forest: ",
            paste0(reference.var[!(reference.var %in% x$covariates)]),
            ". Try any of those: ",paste0(x$covariates, collapse=","),".")
  }
  cl <- match.call()
  cl[["x"]] <- strip(x)
  if(!hasArg(data)) cl[["data"]] <- x$data
  cl[[1L]] <- quote(partialDependence)
  # call partialDependence function on stripped semforest
  eval.parent(cl)
}

#' @method partialDependence semforest_stripped
#' @export
#' @import data.table
partialDependence.semforest_stripped <- function(x, data, reference.var, support = 20, points = NULL, mc = NULL, FUN = "median", ...){
  cl <- match.call()
  cl <- cl[c(1L, which(names(cl) %in% c("data", "reference.var", "support", "points", "mc")
))]
  cl[[1L]] <- str2lang("semtree:::partialDependence_data")
  mp <- eval.parent(cl)
  preds <- data.table::data.table(predict(x, data = mp, type = "pars"))
  mp[,names(mp)[-which(names(mp) %in% c(reference.var, colnames(preds)))]:=NULL]
  mp <- cbind(mp, preds)
  
  # Use median or quantile
  pd_samples <- mp[, do.call("c", lapply(.SD, function(thiscol){
    as.list(do.call(FUN, c(list(thiscol), list(...))))
  })), by = reference.var, .SDcol = attr(x, "parameters")]
  
  ret <- list(samples=pd_samples, reference.var = reference.var, support = support, points = points, FUN = FUN, type = "pd")
  class(ret) <- c("partialDependence", class(ret))
  return(ret)
}

#' Compute partial dependence for latent growth models
#' 
#' Compute the partial dependence of a predictor, or set of predictors,
#' on the predicted trajectory of a latent growth model.
#' 
#' @param x An object for which a method exists
#' @param data Optional \code{data.frame} that was used to train the
#' model.
#' @param reference.var Character vector, referring to the (independent)
#' reference variable or variables for which partial dependence is calculated.
#' Providing two (or more) variables allows for probing interactions, but note
#' that this is computationally expensive.
#' @param support Integer. Number of grid points for interpolating the
#' \code{reference.var}. Alternatively, use \code{points} for one or more
#' variables named in \code{reference.var}.
#' @param points Named list, with elements corresponding to \code{reference.var}
#' . Use this argument to provide specific points for which to obtain marginal
#' dependence values; for example, the mean and +/- 1SD of \code{reference.var}.
#' @param mc Integer. If \code{mc} is not \code{NULL}, the function will sample
#' \code{mc} number of rows from \code{data} with replacement, to estimate 
#' marginal dependency using Monte Carlo integration. This is less
#' computationally expensive.
#' @param FUN Character string with function used to integrate predictions
#' across all elements of \code{x}.
#' @param times Numeric matrix, representing the factor loadings of a latent
#' growth model, with columns equal to the number of growth \code{parameters},
#' and rows equal to the number of measurement occasions.
#' @param parameters Character vector of the names of the growth parameters;
#' defaults to \code{NULL}, which assumes that the growth parameters are the
#' only parameters and are in the correct order.
#' @param ... Extra arguments passed to \code{FUN}.
#' @author Caspar J. Van Lissa
#' @export
partialDependence_growth <- function(x, data, reference.var, support = 20, points = NULL, mc = NULL, FUN = "median", times = NULL, parameters = NULL, ...){
  cl <- match.call()
  cl <- cl[c(1L, which(names(cl) %in% c("data", "reference.var", "support", "points", "mc")
))]
  cl[[1L]] <- str2lang("semtree:::partialDependence_data")
  mp <- eval.parent(cl)
  preds <- predict(x, data = mp, type = "pars", parameters = parameters)
  preds <- data.table(t(apply(preds, 1, .trajectory, L = times)))
  mp[,names(mp)[-which(names(mp) %in% c(reference.var, colnames(preds)))]:=NULL]
  mp <- cbind(mp, preds)
  # Use median or quantile
  out <- mp[, do.call("c", lapply(.SD, function(thiscol){
    as.list(do.call(FUN, c(list(thiscol), list(...))))
  })), by = reference.var]
  out <- melt(out, id.vars = reference.var,
       measure.vars = names(out)[!names(out) %in% reference.var],
       variable.name = "Time")
  Time <- NA # TODO this is a hack to fix the CRAN check issue of "Time" 
  # not being defined
  out[, "Time" := as.integer(as.factor(Time))]
  ret <- list(samples=out, reference.var = reference.var, support = support, points = points, FUN = FUN, type = "growth")
  class(ret) <- c("partialDependence", class(ret))
  return(ret)
}

#' Create dataset to compute partial dependence
#' 
#' Create a dataset with fixed values for \code{reference.var} for all other
#' values of \code{data}, or using \code{mc} random samples from \code{data}
#' (Monte Carlo integration).
#' 
#' @param data The \code{data.frame} that was used to train the
#' model.
#' @param reference.var Character vector, referring to the (independent)
#' reference variable or variables for which partial dependence is calculated.
#' Providing two (or more) variables allows for probing interactions, but note
#' that this is computationally expensive.
#' @param support Integer. Number of grid points for interpolating the
#' \code{reference.var}. Alternatively, use \code{points} for one or more
#' variables named in \code{reference.var}.
#' @param points Named list, with elements corresponding to \code{reference.var}
#' . Use this argument to provide specific points for which to obtain marginal
#' dependence values; for example, the mean and +/- 1SD of \code{reference.var}.
#' @param mc Integer. If \code{mc} is not \code{NULL}, the function will sample
#' \code{mc} number of rows from \code{data} with replacement, to estimate 
#' @param keep_id Boolean. Default is false. Should output contain a row id column?
#' marginal dependency using Monte Carlo integration. This is less
#' computationally expensive.
#' @author Caspar J. Van Lissa
#' @export
partialDependence_data <- function(data, reference.var, support = 20, 
                                   points = NULL, mc = NULL, keep_id = FALSE) {
  if(is.null(points)){
    points <- sapply(reference.var, function(x) {
      seq_unif(data[[x]], length.out = support)
    }, simplify = FALSE)
  } else {
    addthese <- reference.var[!reference.var %in% names(points)]
    points[addthese] <- sapply(addthese, function(x) {
      seq_unif(data[[x]], length.out = support)
    }, simplify = FALSE)
    
  }
  points <- do.call(data.table, c(list(id = 1), expand.grid(points)))
  # if(length(moderator) > 0 & !is.null(mod_levels)){
  #   points[[which(names(points) == moderator)]] <- mod_levels
  # }
  # 
  # points <- data.table(id = 1, expand.grid(points))
  
  if(!is.null(mc)){
    int.points <-
      data.table(id = 1, data[sample(seq_len(nrow(data)), min(mc, nrow(data))), !colnames(data) %in% reference.var, drop = FALSE])
  } else {
    int.points <-
      data.table(id = 1, data[, !colnames(data) %in% reference.var, drop = FALSE])
  }
  out = merge(int.points,
              points,
              all = TRUE,
              allow.cartesian = TRUE)
  
  if (!keep_id) {
    out = out[,!"id", with = FALSE]
  }
  
  setcolorder(out, names(data))
  out
}

seq_unif <- function(x, length.out){
  UseMethod("seq_unif", x)
}
seq_unif.numeric <- function(x, length.out){
  seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = length.out)
}

seq_unif.integer <- function (x, length.out) {
  min.x = min(x, na.rm = TRUE)
  max.x = max(x, na.rm = TRUE)
  unique_vals <- length(unique(x))
  x.length = max.x - min.x
  if (length.out > unique_vals) {
    unique(sort(x))
  }
  else {
    seq.int(min.x, max.x, length.out = length.out)
  }
}

seq_unif.character <- function(x, length.out){
  x.length = length(unique(x))
  if (length.out < x.length) {
    warning("length.out is less than the number of unique values")
  }
  sample(unique(x), size = min(length.out, x.length))
}

seq_unif.factor <- function(x, length.out) {
  x.length = length(unique(x))
  if (length.out >= x.length) {
    sort(unique(x))
  }
  else {
    if (is.ordered(x)) {
      unique(x)[seq_unif(seq_len(x.length), length.out)]
    }
    else {
      warning("length.out is less than the number of levels")
      sort(sample(unique(x), size = length.out))
    }
  }
}

seq_unif.logical <- seq_unif.factor



# Plot methods ------------------------------------------------------------

#' @method plot partialDependence
#' @export
plot.partialDependence <- function(x, y, ...){
  switch(x$type, 
         "growth" = plot_growth(x$samples, ...),
         plot_partialDependence(x = x, ...)
         )
}