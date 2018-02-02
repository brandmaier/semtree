getNodeById <-
function(tree, id)
{
	if (tree$node_id == id) {
		return(tree);
	}
	
	#result <- c();
	if (tree$caption != "TERMINAL")
	{
		l <- getNodeById(tree$left_child, id);
		r <- getNodeById(tree$right_child ,id);
		
		# slow solution
		#result <- append(result,c(l,r));
	  result <- NULL
	  if (!is.null(l)) result <- l
	  if (!is.null(r)) result <- r
			
		return(result);	
	} else{
		return(NULL);
		
	}
	
}
