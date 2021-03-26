#' Merge two SEM forests
#' 
#' This overrides generic base::merge() to merge two forests into one.
#' 
#' 
#' @aliases merge.semforest
#' @param x A SEM Forest
#' @param y A second SEM Forest
#' @param \dots Extra arguments. Currently unused.
#' @author Andreas M. Brandmaier, John J. Prindle
#' @seealso \code{\link{semtree}}
#' @references Brandmaier, A.M., Oertzen, T. v., McArdle, J.J., & Lindenberger,
#' U. (2013). Structural equation model trees. \emph{Psychological Methods},
#' 18(1), 71-86.
#' @exportS3Method merge semforest
merge.semforest <- function(x, y, ...)
{
  return(merge.internal(list(x,y)))
}
  
  
merge.internal <- function(forest.list){
  
  num.forests <- length(forest.list)
  
  forest <- forest.list[[1]]
  for (i in 2:num.forests) {
    
    # some checks
    c1 <- digest::digest(forest$model)==digest::digest(forest.list[[i]]$model)
    tmp1 <- forest$control
    tmp1$num.trees <- NA
    tmp2 <- forest.list[[i]]$control
    tmp2$num.trees <- NA
    c2 <- digest::digest(tmp1)==digest::digest(tmp2)
    if (!c1) {
      stop("Cannot merge forests! Models differ.");
    }
    
    if (!c2) {
      warning("Merging forests with different control objects!");
    }
    
    forest$forest <- c(forest$forest,forest.list[[i]]$forest)
    forest$forest.data <- c(forest$forest.data,forest.list[[i]]$forest.data)
    forest$seeds <- c(forest$forest.seeds,forest.list[[i]]$forest.seeds)
    forest$elapsed <- forest$elapsed+forest.list[[i]]$elapsed
  }
  
  forest$merged <- TRUE
  
  return(forest)
}

