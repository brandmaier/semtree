#
# alternative estimator
# of variable importance with focus parameter
#
# @param tree A SEM tree
# @param data A data.frame
# @param cov.name Character. Name of a predictor for which variable importance is estimated
# @param constraints
#
# @return Numeric
#
varimpFocus <- function(tree, data, cov.name, constraints = NULL)
{
  loglik = tree$control$loglik
  
  has_constraints <- TRUE
  if (is.null(constraints)) { has_constraints <- FALSE } else {
    if (is.null(constraints$focus.parameters)) has_constraints <- FALSE
  }
  
  if (!has_constraints) {
    ui_fail("Forest was not run with focus parameters!")
  }
  
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
  num.evaluated <- 0
  
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
    ll.baseline <- NA
    ll.baseline <-
      try({evaluateDataLikelihood(original.node$model , oob.data[data.rows, , drop = FALSE], loglik=loglik)  }) 
    
    # get loss of fit by resampling
    # ------------------------ 8< -------------------
    
    # obtain focus parameter estimates from resampled node
    resampled.node <- semtree::getNodeById(tree, unique.pairs[i, 2])
    
    # get focus parameter names 
    focus_parameter_names <- constraints$focus.parameters
    
    if (getModelType(resampled.node$model) == "OpenMx") {
    
      # get focus parameter estimates from resampled node
      focus_parameter_values <- omxGetParameters(resampled.node$model)[focus_parameter_names]
      
      # set the focus parameter estimates from resampled model to original model
      temp_model <- omxSetParameters(original.node$model, labels = focus_parameter_names,values = focus_parameter_values)
    
    } else if (getModelType(resampled.node$model) == "lavaan") {
      
      temp_model <- original.node$model
      ids <- temp_model@ParTable$label == focus_parameter_names
      if (!any(ids)) ui_fail("Error with focus parameter specification!")
      
      focus_parameter_values <- resampled.node$model@parTable$est[ids]
      
      temp_model@parTable$est[ids] <- focus_parameter_values
      
    } else {
      ui_fail("Focus variable importance not implemented for this type of model.")
    }
    
    # re-evaluate data likelihood (NA if fit job exits)
    ll.focus <- NA
    ll.diff <- NA
    try({
      ll.focus <- evaluateDataLikelihood(temp_model, oob.data[data.rows, , drop = FALSE], loglik=loglik)
      ll.diff <- ll.focus - ll.baseline
    })
    if (is.na(ll.focus)) num.failed <- num.failed + 1
    
    
    # ------------------------ 8< -------------
    
    num.evaluated <- num.evaluated + 1
    
    # sum of loss of fit
    total <- total + ll.diff
  }
  
  # check whether some tests failed
  
  if (num.failed > 0) {
    num.total <- nrow(ids)
    percentage <- round(num.failed / num.total * 100)
    ui_warn(
      "A total of ",
      num.failed, " out of ", num.evaluated, 
      " joint models could not be evaluated due to model fit errors for covariate ",cov.name, " in tree",tree$name ,"."
    )
  }
  
  return(list(ll.diff=total,num.failed=num.failed))
}