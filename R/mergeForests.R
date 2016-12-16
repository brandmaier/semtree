#require("digest")

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
    c2 <- digest::digest(forest$control)==digest::digest(forest.list[[i]]$control)
    if (!c1 || !c2) {
      stop("Cannot merge forests! Models or control objects differ");
    }
    
    forest$forest <- c(forest$forest,forest.list[[i]]$forest)
    forest$forest.data <- c(forest$forest.data,forest.list[[i]]$forest.data)
    forest$seeds <- c(forest$forest.seeds,forest.list[[i]]$forest.seeds)
    forest$elapsed <- forest$elapsed+forest.list[[i]]$elapsed
  }
  
  forest$merged <- TRUE
  
  return(forest)
}

