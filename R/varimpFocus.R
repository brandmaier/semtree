varimpFocus <- function(tree, data, cov.name)
{
  control <- tree$control 
  constraints <- tree$constraints
 
  oob.data <- data$oob.data
  #cov.name <- "grp1"
  #cov.name <- "grp2"
  #cov.name <- "noise"
  permutation.idx <- which(cov.name == names(oob.data))
  tree <- forest$forest[[1]]
  col.data <- oob.data[, permutation.idx]
  
  oob.data.permuted <- oob.data
  oob.data.permuted[, permutation.idx] <-
    sample(col.data, length(col.data), replace = F)
  
  ids <- cbind(traverse(tree, oob.data),traverse(tree, oob.data.permuted))
  colnames(ids) <- c("Original","Permuted")
  
 
  # create pairwise fit matrix
  temp.model <- list()
  list.of.leaves <- getLeafs(tree)
  model <- tree$model
  for (i in 1:length(list.of.leaves)) {
    for (j in 1:length(list.of.leaves)) {
      #cat("Testing ",i," ",j,"\n")
      if (i>=j) next;
      sub1 <- list.of.leaves[[i]]$model$data$observed
      sub2 <- list.of.leaves[[j]]$model$data$observed
      focus.param.models <- fitSubmodels(model, sub1, sub2, 
                                         control, invariance=constraints$focus.parameters,
                                         return.models = TRUE) 
      temp.model[[paste0( list.of.leaves[[i]]$node_id,"-", list.of.leaves[[j]]$node_id)]]<-focus.param.models
    }
  }
  
  
  # compute loss in fit from original to joint model
  total <-0
  for (i in 1:nrow(ids)) {
    original.id <- ids[i,1]
    original.node <- semtree::getNodeById(tree, original.id)
    ll.baseline <- evaluateDataLikelihood( original.node$model , oob.data[i,,drop=FALSE])
    
    #temp.model <- getPairwiseModel(modelmat, ids[i,1], ids[i,2])
    if (ids[i,1]==ids[i,2]) next;
    ident <- paste0(min(ids[i,]),"-",max(ids[i,]))
    fmodel <- temp.model[[ident]]
    
    if (ids[i,1]<ids[i,2]) {
      ffmodel <- fmodel$model1
    } else {
      ffmodel <- fmodel$model2
    }
    
    ll.focus <- evaluateDataLikelihood( ffmodel , oob.data[i,,drop=FALSE])
    
    ll.diff <- ll.focus - ll.baseline
    
    total <- total + ll.diff
    
    #cat("LLB: ",ll.baseline,"\t")
    #cat("LLF: ",ll.focus,"\t")
    #cat("Diff:",ll.diff,"\n")
  }
  #cat("TOTAL: ",total,"\n")
  
  return(total)
}