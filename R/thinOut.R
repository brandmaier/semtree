#' Thin Out a Forest
#' 
#' This is a function that removes all "empty" trees, that is, those
#' that contain only the root node and no splits
#' 
#' @parameter x A SEM forest
#' 
#' @export
thinOut <- function(x)
{
  stopifnot(inherits(x,"semforest"))
  
  is.root <- sapply(X=x$forest, FUN=getHeight)==0
  
  if (sum(is.root)>0) {
    x$forest = x$forest[-is.root]
    x$forest.data = x$forest.data[-is.root]
    cat("A total of ",sum(is.root)," trees were thinned out.")
  } else {
    cat("No trees were thinned out.")
  }
  
  return(x)
}
