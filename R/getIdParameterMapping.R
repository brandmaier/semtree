# returns a table with a list of node ids and corresponding parameter estimates
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
	#print(toString(tree$params))
	return(rbind(v,r,l));
	
}
