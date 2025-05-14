varimpTree <- function(tree,
                       data,
                       var.names = NULL,
                       verbose = FALSE,
                       max.level = NA,
                       eval.fun = evaluateTree,
                       method = "permutation",
                       conditional = FALSE,
                       constraints = NULL,
                       loglik = "model") {
  # prune tree back to given level if "max.level" is specified
  if (!is.na(max.level)) {
    tree <- prune(tree, max.level)
  }
  
  # prepare storage for results
  total <- rep(0, length(var.names))
  
  oob.data <- data$oob.data
  
  # obtain likelihood of unpermuted data
  ll.baseline <- eval.fun(tree, oob.data)$deviance
  
  # get all predictors that appeared in the tree
  treecovs <- getCovariatesFromTree(tree)
  
  # cycle through all covariates
  for (cov.name in var.names) {
    if (verbose) {
      ui_message("- Testing ", cov.name, "\n")
    }
    
    index <- which(var.names == cov.name)
    
    if (!cov.name %in% treecovs) {
      total[index] <- NA
      
      next
      
    } else {
      # permute variable with name "cov.name" in data and store result in "oob.data.permuted"
      permutation.idx <- which(cov.name == names(data$oob.data))
      oob.data.permuted <- oob.data
      
      # random permutation
      if (!conditional) {
        col.data <- oob.data.permuted[, permutation.idx]
        oob.data.permuted[, permutation.idx] <-
          sample(col.data, length(col.data), replace = FALSE)
      } else {
        stop("Not yet implemented!")
        
        oob.data.permuted <-
          conditionalSample(tree, oob.data.permuted, permutation.idx)
        
        if (all(oob.data.permuted == oob.data)) {
          ui_warn("Warning! OOB DATA RESAMPLING HAD NO EFFECT")
        }
        

        # stop("Not implemented yet!")
      }
      
      # obtain likelihood of permuted data
      if (method == "permutation") {

        ll.permuted <- eval.fun(tree, oob.data.permuted, loglik = loglik)$deviance
        ll.diff <- -ll.baseline + ll.permuted
        
      } else if (method == "permutationInteraction") {
        ll.permuted <-
          evaluateTreeConditional(tree, list(oob.data.permuted, oob.data))$deviance
        ll.diff <- -ll.baseline + ll.permuted
      } else if (method == "permutationFocus") {
        ll.diff <-
          varimpFocus(
            tree = tree,
            data = data,
            cov.name = cov.name,
            constraints = constraints
          )
      } else {
        stop(paste("Error. Method is not implemented: ", method))
      }
      
      
      if (verbose) {
        ui_info(cov.name, "LL Difference", ll.diff, "\n")
      }
      
      total[index] <- ll.diff
    }
    
  }
  
  return(list(total = total, ll.baseline = ll.baseline))
  
}
