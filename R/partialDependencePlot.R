
partialDependencePlot <- function(forest, reference.var, reference.param, support=10, xlab=NULL, ylab=NULL,...)  {
  .Deprecated("partialDependence")
  pd <- partialDependence(forest, reference.var, reference.param, support)
  plot(pd, xlab, ylab, ...)
  return(pd)
}

partialDependence <- function(forest, reference.var, reference.param, support=NULL) 
{
  
  result <- list()
  
  if (!reference.var %in% names(forest$data)) {
    stop("Reference variable is not in the dataset")
  }
  
  model.params <- names(OpenMx::omxGetParameters(forest$model))
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
    
    xgrid <- levels(refVar)
    
    
  } else {
    
    if (is.null(support)) {
      support <- 10
    }
    
    start <- min(refVar, na.rm=TRUE)
    end <- max(refVar, na.rm=TRUE)
    
    xgrid <- seq(start, end, length=support)   
  }

  
  fd <- partialDependenceDataset(forest$data, reference.var, xgrid)
  
  dict <- list()
  for (elem in xgrid) {
    dict[[as.character(elem)]] <- NA
  }
  
  # traverse
  for (i in 1:length(forest$forest)) {
    tree <- forest$forest[[i]]
    leaf.ids <- traverse( tree, fd)
    for (j in 1:length(leaf.ids)) {
      model <-getNodeById(tree, leaf.ids[j])
      p <- model$params[param.id]    
      yvalue <- fd[j, reference.var]
      
      dict[[as.character(yvalue)]] <- c(dict[[as.character(yvalue)]],p)
    }
  }
  

  result$reference.var <- reference.var
  result$reference.param <- reference.param

  result$dict <- dict
  result$xgrid <- xgrid
  
  class(result) <- "partialDependence"
  
  return(result)
}


plot.partialDependence <- function(x, type="l",xlab=NULL, ylab=NULL, xlim=NULL, ylim=NULL, ...)
{
  #if (!(x inherits ("partialDependence"))) {
  #  stop("Invalid x object not of class partialDependence");
  #}
  
  if (is.null(xlab)) {
    xlab <- x$reference.var
  }
  
  if (is.null(ylab)) {
    ylab <- x$reference.param
  }
  
  # collect
  col1 <- x$xgrid
  col2 <- rep(NA, length(col1))
  for (i in 1: length(col1)) {
    col2[i] <- mean(x$dict[[as.character(col1[i])]], na.rm=TRUE)
  }
  plot(col1, col2, type=type, xlab=xlab,ylab=ylab, xlim=xlim, ylim=ylim, ...)
  
}