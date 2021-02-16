#' Tree Size
#' 
#' Counts the number of nodes in a tree.
#' 
#' 
#' @param tree A SEM tree object.
#' @author Andreas M. Brandmaier, John J. Prindle
#' @references Brandmaier, A.M., Oertzen, T. v., McArdle, J.J., & Lindenberger,
#' U. (2013). Structural equation model trees. \emph{Psychological Methods},
#' 18(1), 71-86.
getNumNodes <-
function(tree)
{
	if ((is.null(tree$left_child)) && (is.null(tree$right_child))) 	
	{
		return(1);
	}

	count <- 1
	
	if (tree$left_child$caption != "TERMINAL") {
		count <- count + getNumNodes(tree$left_child)
	} else {
		count <- count + 1	
	}
	if (tree$right_child$caption != "TERMINAL") {
		count <- count + getNumNodes(tree$right_child)
	} else {
		count <- count + 1	
	}	
	
	return(count)
  
}
