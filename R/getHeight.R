#' Determine Height of a Tree
#' 
#' Returns height of a SEM Tree, which equals to the length of the longest path
#' from root to a terminal node.
#' 
#' 
#' @param tree A SEM tree.
#' @author Andreas M. Brandmaier, John J. Prindle
#' @references Brandmaier, A.M., Oertzen, T. v., McArdle, J.J., & Lindenberger,
#' U. (2013). Structural equation model trees. \emph{Psychological Methods},
#' 18(1), 71-86.

getHeight <- function(tree)
{
  if ((is.null(tree$left_child)) && (is.null(tree$right_child))) 	
  {
    return(1);
  }
  
  if (tree$left_child$caption != "TERMINAL") {
    countl <- 1+ getHeight(tree$left_child)
  } else {
    countl <- 2	
  }
  if (tree$right_child$caption != "TERMINAL") {
    countr <- 1+ getHeight(tree$right_child)
  } else {
    countr <- 2	
  }	
  
  return( max(countl, countr) )
}



#' Get the depth (or, height) a tree.
#' 
#' Returns the length of the longest path from a root node to a leaf node.
#' 
#' 
#' @param tree A \code{\link{semtree}} object
#' @author Andreas M. Brandmaier
#' @references Brandmaier, A.M., Oertzen, T. v., McArdle, J.J., & Lindenberger,
#' U. (2013). Structural equation model trees. \emph{Psychological Methods},
#' 18(1), 71-86.
#' @export
getDepth <- getHeight
