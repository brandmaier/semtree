#' @exportS3Method prune semtree
prune.semforest <- function(object, max.depth=NULL, num.trees=NULL, ...)
{
  if (!is.null(num.trees)) {
    object$forest <- object$forest[1:num.trees]
    object$forest.data <- object$forest.data[1:num.trees]
  }
  
  if (!is.null(max.depth)) {
    object$forest <- lapply(object$forest, prune.semtree, max.depth=max.depth)
  }
  
  return(object);
  
}