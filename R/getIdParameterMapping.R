#' This function recursively goes trought the tree and returns
#' a table with node ids in the first column and parameter estimates
#' in the remaining columns. This can be used to present SEM tree
#' estimates in tabular form
#'
#' @tree A SEM tree
#' @returns a table with a list of node ids and corresponding parameter estimates
getIdParameterMapping <-
function(tree)
{
	v <- cbind(tree$node_id, t(tree$params));
	
	if (tree$caption == "TERMINAL")
	{
		return(v);
	}
	

	r <- getIdParameterMapping(tree$right_child);
	l <- getIdParameterMapping(tree$left_child);

	return(rbind(v,r,l));
	
}
