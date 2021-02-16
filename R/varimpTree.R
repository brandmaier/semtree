varimpTree <- function(tree,
                       data,
                       var.names = NULL,
                       verbose = FALSE,
                       max.level = NA,
                       eval.fun = evaluateTree,
                       method = "permutation",
                       conditional = FALSE) {
  # prune tree back to given level if "max.level" is specified
  if (!is.na(max.level)) {
    tree <- prune(tree, max.level)
  }
  
  # prepare storage for results
  total <- rep(0, length(var.names))
  
  oob.data <- data$oob.data
  
  # obtain likelihood of unpermuted data
  ll.baseline <- eval.fun(tree, oob.data)$deviance
  #if (verbose) {
  #  ui_info("LL baseline", ll.baseline, "\n")
  #}
  
  
  # get all predictors that appeared in the tree
  treecovs <- getCovariatesFromTree(tree)
  
  # preparation for focus importance
  if (method == "permutationFocus") {
    if (verbose) {
      ui_message("Pre-computing focus models for tree ", tree$name)
    }
    num.failed = 0
    # create pairwise fit matrix
    joint.model.list <- list()
    list.of.leaves <- getLeafs(tree)
    model <- tree$model
    for (i in 1:length(list.of.leaves)) {
      for (j in 1:length(list.of.leaves)) {
        #cat("Testing index=",i," ",j,"\t node ids=",list.of.leaves[[i]]$node_id,"-",list.of.leaves[[j]]$node_id,"\n")
        if (i >= j)
          next
        
        sub1 <- list.of.leaves[[i]]$model$data$observed
        sub2 <- list.of.leaves[[j]]$model$data$observed
        
        tc <- tree$control
        if ((nrow(sub1) < tree$control$min.bucket) | (nrow(sub2) < tree$control$min.bucket)) {
          tc$min.bucket <- min(nrow(sub1),nrow(sub2))
          ui_warn("Bucket size parameter was adjusted from ", tree$control$min.bucket," to ", tc$min.bucket)
        }
        temp.N <- nrow(sub1)+nrow(sub2)
        
        
        focus.param.models <- fitSubmodels(
          model,
          sub1,
          sub2,
          tc,
          invariance = tree$constraints$focus.parameters,
          return.models = TRUE
        )
        
        if (is.null(focus.param.models) | (all(is.na(focus.param.models)))) {
          ui_fail("Model did not converge")
          num_failed = num.failed + 1
        }
        
        joint.model.list[[paste0(list.of.leaves[[i]]$node_id,
                                 "-",
                                 list.of.leaves[[j]]$node_id)]] <-
          focus.param.models
      }
    }
  }
  
  # all covariates
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
      
      # browser()
      
      # random permutation
      if (!conditional) {
        col.data <- oob.data.permuted[, permutation.idx]
        oob.data.permuted[, permutation.idx] <-
          sample(col.data, length(col.data), replace = F)
      } else {
        stop("Not yet implemented!")
        
        oob.data.permuted <-
          conditionalSample(tree, oob.data.permuted, permutation.idx)
        
        if (all(oob.data.permuted == oob.data)) {
          ui_warn("Warning! OOB DATA RESAMPLING HAD NO EFFECT")
        }
        
        #browser()
        # stop("Not implemented yet!")
      }
      
      # obtain likelihood of permuted data
      if (method == "permutation") {
        ll.permuted <- eval.fun(tree, oob.data.permuted)$deviance
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
            joint.model.list
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
  
  
  #		}, error=function(e) {
  #			cat(paste("Error in tree #","\n",e));
  #			return(list(total=rep(NA, length(var.names)),ll.baseline=NA));
  #	});
  
}
