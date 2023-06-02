#
# alternative estimator
# of variable importance with focus parameter
#
# 
#
varimpFocus <- function(tree, data, cov.name, joint.model.list, constraints = NULL)
{
  
  oob.data <- data$oob.data
  
  permutation.idx <- which(cov.name == names(oob.data))
  
  col.data <- oob.data[, permutation.idx]
  
  oob.data.permuted <- oob.data
  oob.data.permuted[, permutation.idx] <-
    sample(col.data, length(col.data), replace = F)
  
  # generate a matrix of node ids per observation
  # that correspond to the original data and permuted data
  ids <-
    cbind(traverse(tree, oob.data), traverse(tree, oob.data.permuted))
  colnames(ids) <- c("Original", "Permuted")
  
  # compute loss in fit from original to joint model
  total <- 0
  num.failed <- 0
  
  unique.pairs <- unique(ids)
  
  for (i in 1:nrow(unique.pairs)) {
    
    id1 <- unique.pairs[i, 1] # original node id
    id2 <- unique.pairs[i, 2] # resampled node id
    # skip if both ids are identical
    if (id1 == id2)
      next
    
    # collect data rows with ids that match this pair
    data.rows <- which(apply(ids,1, function(x){all(x==unique.pairs[i,])}))
    
    # get model and evaluate baseline likelihood of data in original node
    original.id <- unique.pairs[i, 1]
    original.node <- semtree::getNodeById(tree, original.id)
    ll.baseline <-
      evaluateDataLikelihood(original.node$model , oob.data[data.rows, , drop = FALSE])   
    
    # get loss of fit by resampling
    # ------------------------ 8< -------------------
    
    # obtain focus parameter estimates from resampled node
    resampled.node <- semtree::getNodeById(tree, unique.pairs[i, 2])
    
    # get focus parameter names 
    focus_parameter_names <- constraints$focus.parameters
    
    # get focus parameter estimates from resampled node
    focus_parameter_values <- omxGetParameters(resampled.node$model)[focus_parameter_names]
    
    ui_info("Setting ", focus_parameter_names, " to ", focus_parameter_values,"\n")
    flush.console()
    
    # set the focus parameter estimates from resampled model to original model
    temp_model <- omxSetParameters(original.node$model, labels = focus_parameter_names,values = focus_parameter_values)
    
    # re-evaluate data likelihood
    ll.focus <- evaluateDataLikelihood(temp_model, oob.data[data.rows, , drop = FALSE])
    
    ll.diff <- ll.focus - ll.baseline
    # ------------------------ 8< -------------
    
    # sum of loss of fit
    total <- total + ll.diff
  }
  
  # check whether some tests failed
  
  if (num.failed > 0) {
    num.total <- nrow(ids)
    percentage <- round(num.failed / num.total * 100)
    ui_warn(
      "Warning. A total of ",
      num.failed,
      " joint models could not be evaluated. Importance values are possibly biased."
    )
  }
  
  return(total)
}