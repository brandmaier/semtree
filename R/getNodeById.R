getNodeById <-
function(tree, id)
{
	if (tree$node_id == id) {
		return(tree);
	}
	
	result <- c();
	if (tree$caption != "TERMINAL")
	{
		l <- getNodeById(tree$left_child, id);
		r <- getNodeById(tree$right_child ,id);
		result <- append(result,c(l,r));
		return(result);	
	} else{
		return(c());
		
	}
	
}
