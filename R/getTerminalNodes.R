#' Returns all leafs of a tree
#' 
#' Returns all leafs (=terminal nodes) of a tree.
#' 
#' 
#' @param tree A semtree object.
#' @author Andreas M. Brandmaier, John J. Prindle
#' @references Brandmaier, A.M., Oertzen, T. v., McArdle, J.J., & Lindenberger,
#' U. (2013). Structural equation model trees. \emph{Psychological Methods},
#' 18(1), 71-86.
#' @export
getTerminalNodes <-
function(tree)
{
	
	data <- getTerminalNodes.rec(tree, 0)
	
	data <- data.frame(data[,-1, drop=FALSE], row.names=data[,1])
	names(data) <- tree$param_names;
	data <- data[order(as.numeric(rownames(data))),]
	
		return(data);
}


getTerminalNodes.rec <-
function(tree, level = 0)
{
	v <- NULL
	if (tree$caption == "TERMINAL")
	{
		v <- cbind(tree$node_id, t(tree$params));
		return(v);
	}
	

	r <- getTerminalNodes.rec(tree$right_child, level+1);
	l <- getTerminalNodes.rec(tree$left_child, level+1);

	data <- rbind(v,r,l);


	return(data);
	
}
