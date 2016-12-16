subforest <- function(forest, num=NULL, type="nonnull", from=1)
{
  
  if (type=="random") {
    ids <- sample(1:length(forest$forest),size = num,replace=FALSE)
  } else if (type=="nonnull") {
    nulls <- sapply(forest$forest,is.null)
    if (sum(nulls) != 0) {
      ids <- which(!nulls)
    } else {
      return(forest);
    }
  } else {
    
    if(is.null(num)) {return(NULL);}
    ids <- from:(from+num)
  }
  
  forest$forest <- forest$forest[ids]
  forest$forest.data <- forest$forest.data[ids]
  
  return(forest);
}