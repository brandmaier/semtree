#' @exportS3Method prune semforest
prune.semforest <- function(object, num.trees=NULL, ...)
{
  if (!is.null(num.trees)) {
    object$forest <- object$forest[1:num.trees]
    object$forest.data <- object$forest.data[1:num.trees]
  }
 
  # apply tree-wise pruning, pass ... arguments to
  # prune.semtree
  dots <- list(...)
  object$forest<-lapply(object$forest, function(z) {
    do.call(prune.semtree, c(list(object=z),list(...)))
  })
  
 
 # object$forest <- lapply(object$forest, prune.semtree, max.depth=max.depth, converged=converged)

  
  return(object);
  
}