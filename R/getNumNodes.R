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
