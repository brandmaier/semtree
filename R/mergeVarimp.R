#' @exportS3Method merge semforest.varimp
merge.semforest.varimp <- function(x, y, ...){
  return(merge_internal_varimp(list(x, y)))
}

merge_internal_varimp <- function (varimp_list) 
{
  numtrees <- sapply(varimp_list, function(x){length(x$ll.baselines)})
  numfeatures <- sapply(varimp_list, function(x){dim(x$importance)[2]})
  if(length(unique(numfeatures)) > 1) stop("Not all semforest.varimp objects have the same number of variables.")
  varnames <- sapply(varimp_list, `[[`, "var.names")
  if(any(apply(varnames, 1, function(x){length(unique(x))}) > 1)) stop("Not all var.names are the same.")
  out <- list(
    ll.baselines = vector("numeric", sum(numtrees)),
    importance = matrix(nrow = sum(numtrees), ncol = numfeatures[1]),
    elapsed = varimp_list[[1]]$elapsed,
    var.names = varimp_list[[1]]$var.names
  )
  colnames(out$importance) <- colnames(varimp_list[[1]]$importance)
  for (i in 2:length(varimp_list)) {
    out$elapsed <- out$elapsed + varimp_list[[i]]$elapsed
  }
  index_trees <- c(0, numtrees, 0)
  for (i in 1:length(varimp_list)) {
    indcs <- (sum(index_trees[1:i])+1):sum(index_trees[1:i+1])
    out$ll.baselines[indcs] <- varimp_list[[i]]$ll.baselines
    out$importance[indcs, 1:numfeatures[1]] <- varimp_list[[i]]$importance
  }
  class(out) <- class(varimp_list[[1]])
  return(out)
}