#' Determine Height of a Tree
#'
#' Returns height of a SEM Tree, which equals to the length of the longest path
#' from root to a terminal node.
#' 
#' Example:
#' A SEM tree with only the root node has depth 0.
#' A SEM tree with one decision node has depth 1.
#'
#'
#' @param tree A SEM tree.
#' @author Andreas M. Brandmaier, John J. Prindle
#' @references Brandmaier, A.M., Oertzen, T. v., McArdle, J.J., & Lindenberger,
#' U. (2013). Structural equation model trees. \emph{Psychological Methods},
#' 18(1), 71-86.

getHeight <- function(tree) {
  # if node has no children:
  if ((is.null(tree$left_child)) && (is.null(tree$right_child))) {
    return(0)
  }

  if (!is.null(tree$left_child)) {
    countl <- 1 + getHeight(tree$left_child)
  }

  if (!is.null(tree$right_child$caption)) {
    countr <- 1 + getHeight(tree$right_child)
  } 

  return(max(countl, countr))
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
