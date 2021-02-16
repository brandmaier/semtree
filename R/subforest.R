#' Creates subsets of trees from forests
#' 
#' Creates subsets of a forest. This can be used to subset a number of trees,
#' e.g. from:(from+num), or to remove all null (type="nonnull") trees that were
#' due to errors, or to randomly select a sub forest (type=random).
#' 
#' 
#' @param forest A SEM Forest object.
#' @param num Number of trees to select.
#' @param type Either 'random' or 'nonnull' or NULL. First selects a random
#' subset, second selects all non-null trees, third allows subsetting trees.
#' @param from Starting index if type=NULL.
#' @author Andreas M. Brandmaier, John J. Prindle
#' @references Brandmaier, A.M., Oertzen, T. v., McArdle, J.J., & Lindenberger,
#' U. (2013). Structural equation model trees. \emph{Psychological Methods},
#' 18(1), 71-86.
#' @export
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
