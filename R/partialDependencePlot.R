partialDependencePlot <- function(forest, reference.var, reference.param, support=10, xlab=NULL, ylab=NULL,...)  {
  .Deprecated("partialDependence")
  pd <- partialDependence(forest, reference.var, reference.param, support)
  plot(pd, xlab, ylab, ...)
  return(pd)
}



#' Partial Dependence Plot
#' 
#' Partial dependence plot for the effect of an indepedent variable in the
#' dataset on the dependent outcome parameter selected. Returns a
#' partialDependence object that can be plotted via generic plot command.
#' 
#' 
#' @aliases partialDependence plot.partialDependence partialDependencePlot
#' @param forest A SEM forest
#' @param reference.var Label of the (independent) reference variable for which
#' partial dependence is plotted
#' @param reference.param Label of the (dependent) model parameter for which
#' partial dependence is plotted
#' @param support Number of grid points for interpolating the reference.var
#' @author Andreas M. Brandmaier
#' @export
partialDependence <- function(forest, reference.var, reference.param, support=NULL)
{
  
  result <- list()
  
  if (!reference.var %in% names(forest$data)) {
    stop("Reference variable is not in the dataset")
  }
  
  model <- forest$model
  if (inherits(model,"MxModel") || inherits(model,"MxRAMModel")) {
    model.params <- names(OpenMx::omxGetParameters(forest$model))
  } else if (inherits(model,"lavaan")) {
    #if (!is.numeric(reference.param)) {
    #  stop("Please specify numeric parameter identifier")
    #}
    
    # TODO: ERROR PRONE
    if (is.null(forest$forest[[1]])) {stop("Error! First tree is NULL")}
    model.params <- forest$forest[[1]]$param_names
  } else {
    stop("Not supported!")
  }
  
  if (!reference.param %in% model.params) {
    stop("Reference parameter is not in the model")
  }
  
  param.id <- which(model.params==reference.param)
  
  refVar <- forest$data[, reference.var]
  
  # factors are mapped onto their levels
  isfac <- FALSE
  if (is.factor(refVar)) {
    #refVar <- levels(refVar)
    isfac <- TRUE
  } 
  
  if (isfac) {
    
    xgrid <- levels(refVar) #unique(unclass(refVar))
    xlabs <- levels(refVar)
    
  } else {
    
    if (is.null(support)) {
      support <- 10
    }
    
    start <- min(refVar, na.rm=TRUE)
    end <- max(refVar, na.rm=TRUE)
    
    xgrid <- seq(start, end, length=support)  
    
    xlabs <- xgrid
  }

  
  fd <- partialDependenceDataset(forest$data, reference.var, xgrid)

  
  # traverse
#  for (i in 1:length(forest$forest)) {
  mapreduce <- function(tree) {
    #tree <- forest$forest[[i]]

    leaf.ids <- traverse( tree, fd)
    ret <- vector("list", length(leaf.ids))
    for (j in 1:length(leaf.ids)) {
      node <-getNodeById(tree, leaf.ids[j])
      p.estimate <- node$params[param.id]    
      yvalue <- fd[j, reference.var]
      
      #dict[[as.character(yvalue)]] <- c(dict[[as.character(yvalue)]],p)
      ret[[j]] <- (list(key=as.character(yvalue), value=p.estimate))
    }
    return(ret)
  }
  
  mapresult <- future.apply::future_lapply(FUN=mapreduce,X=forest$forest)
  
  #result <- list()
  #for (i in 1:10) {
  #  result <- mapreduce(forest$forest[[i]])
  #}
  
  
  dict <- list()
  dictsq <- list()
  cnt <- list()
  for (elem in xgrid) {
    dict[[as.character(elem)]] <- 0
    dictsq[[as.character(elem)]] <- 0
    cnt[[as.character(elem)]] <- 0
  }
  
  
  for (i in 1:length(mapresult)) {
    mr <- simplify2array(mapresult[[i]])
    for (j in 1:dim(mr)[2]) {
      key <- mr[,j]$key
      val <- mr[,j]$value
      dict[[key]] <- dict[[key]]+ val
      dictsq[[key]] <- dictsq[[key]]+ val**2
      cnt[[key]] <- cnt[[key]]+1
    }
  }
  
  
  
  for (elem in xgrid) {
    dict[[as.character(elem)]] <- dict[[as.character(elem)]] / cnt[[as.character(elem)]]
    dictsq[[as.character(elem)]] <- dictsq[[as.character(elem)]] / cnt[[as.character(elem)]]
  }
  
  result$reference.var <- reference.var
  result$reference.param <- reference.param

  result$dict <- dict
  result$dictsq <- dictsq
  result$xgrid <- xgrid
  result$xlabs <- xlabs
  
  result$is.factor <- isfac
  
  class(result) <- "partialDependence"
  
  return(result)
}
