#' Test whether a semtree object is a leaf.
#' 
#' Tests whether a semtree object is a leaf. Returns TRUE or FALSE.
#' 
#' 
#' @param tree A \code{\link{semtree}} object
#' @author Andreas M. Brandmaier
#' @references Brandmaier, A.M., Oertzen, T. v., McArdle, J.J., & Lindenberger,
#' U. (2013). Structural equation model trees. \emph{Psychological Methods},
#' 18(1), 71-86.
#' @export
isLeaf <- function(tree)
{
	if (!inherits(tree,"semtree")) return(NULL)
	return(tree$caption=="TERMINAL")
}
