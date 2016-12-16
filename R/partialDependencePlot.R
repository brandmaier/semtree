
partialDependencePlot <- function(forest, reference.var, reference.param, support=10, xlab=NULL, ylab=NULL,...) 
{
  if (!reference.var %in% names(forest$data)) {
    stop("Reference variable is not in the dataset")
  }
  
  model.params <- names(OpenMx::omxGetParameters(forest$model))
  if (!reference.param %in% model.params) {
    stop("Reference parameter is not in the model")
  }
  
  param.id <- which(model.params==reference.param)
  
  refVar <- forest$data[, reference.var]
  start <- min(refVar)
  end <- max(refVar)
  xgrid <- seq(start, end, length=support)
  fd <- partialDependenceDataset(forest$data, reference.var, support)
  
  dict <- list()
  for (elem in xgrid) {
    dict[[as.character(elem)]] <- NA
  }
  
  # traverse
  for (i in 1:length(forest$forest)) {
    #i <-1
    tree <- forest$forest[[i]]
    leaf.ids <- traverse( tree, fd)
    for (j in 1:length(leaf.ids)) {
      model <-getNodeById(tree, leaf.ids[j])
      p <- model$params[param.id]    
      yvalue <- fd[j, reference.var]
      
      dict[[as.character(yvalue)]] <- c(dict[[as.character(yvalue)]],p)
    }
  }
  
  if (is.null(xlab)) {
    xlab <- reference.var
  }
  
  if (is.null(ylab)) {
    ylab <- reference.param
  }
  
  # collect
  col1 <- xgrid
  col2 <- rep(NA, length(col1))
  for (i in 1: length(col1)) {
    col2[i] <- mean(dict[[as.character(col1[i])]], na.rm=TRUE)
  }
  plot(col1, col2, type="l", xlab=xlab,ylab=ylab, ...)

  
  class(dict) <- "partialDependence"
  
  return(dict)
}