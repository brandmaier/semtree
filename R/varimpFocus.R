varimpFocus <- function(tree, data, cov.name, joint.model.list)
{
  
  oob.data <- data$oob.data

  permutation.idx <- which(cov.name == names(oob.data))
  tree <- forest$forest[[1]]
  col.data <- oob.data[, permutation.idx]
  
  oob.data.permuted <- oob.data
  oob.data.permuted[, permutation.idx] <-
    sample(col.data, length(col.data), replace = F)
  
  ids <- cbind(traverse(tree, oob.data),traverse(tree, oob.data.permuted))
  colnames(ids) <- c("Original","Permuted")
  
 

  
  
  # compute loss in fit from original to joint model
  total <-0
  num.failed <- 0
  for (i in 1:nrow(ids)) {
    original.id <- ids[i,1]
    original.node <- semtree::getNodeById(tree, original.id)
    ll.baseline <- evaluateDataLikelihood( original.node$model , oob.data[i,,drop=FALSE])
    

    if (ids[i,1]==ids[i,2]) next;
    ident <- paste0(min(ids[i,]),"-",max(ids[i,]))
    fmodel <- joint.model.list[[ident]]
    
    if (ids[i,1]<ids[i,2]) {
      ffmodel <- fmodel$model1
    } else {
      ffmodel <- fmodel$model2
    }
    
    if (!is.null(ffmodel)) {
      ll.focus <- evaluateDataLikelihood( ffmodel , oob.data[i,,drop=FALSE])
    
      ll.diff <- ll.focus - ll.baseline
    
    } else {
      num.failed = num.failed + 1
      ll.diff <- 0
    }
    
    total <- total + ll.diff
    
    #cat("LLB: ",ll.baseline,"\t")
    #cat("LLF: ",ll.focus,"\t")
    #cat("Diff:",ll.diff,"\n")
  }
  #cat("TOTAL: ",total,"\n")
  
  if (num.failed > 0) {
    ui_warn("Warning. A total of ", num.failed, "joint models could not be evaluated. Importance values are possibly biased.")
  }
  
  return(total)
}