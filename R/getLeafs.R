#' Get a list of all leafs in a tree
#' 
#' Get a list of all leafs in a tree by recursively searching the tree starting
#' at the given node (if not \code{data} object is given. If \code{data} is
#' given, the function returns the leafs that are predicted for each row of the
#' given data.
#' 
#' 
#' @param tree A \code{\link{semtree}} object
#' @param data A \code{data.frame}
#' @author Andreas M. Brandmaier
#' @references Brandmaier, A.M., Oertzen, T. v., McArdle, J.J., & Lindenberger,
#' U. (2013). Structural equation model trees. \emph{Psychological Methods},
#' 18(1), 71-86.
#' @export
getLeafs <- function(tree, data=NULL) {
  if (!is.null(data)) {
   return(traverse(tree, data))
  }

  if (!isLeaf(tree)) {
    left.list <- getLeafs(tree$left_child)
    right.list <- getLeafs(tree$right_child)
    return( c(left.list,right.list) )
  } else {
    return(list(tree))
  }
}
