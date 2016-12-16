se <- function(tree, leafs.only = TRUE) {

	data <- se.rec(tree, leafs.only, 0)

#	if (level == 0)
#	{
		data <- round(data.frame(data[,-1], row.names=data[,1]),digits=3);
		names(data) <- tree$param_names;
		data <- t(data)
#	}

	return(data);	
}

se.rec <- function(tree, leafs.only=TRUE, level = 0)
{
	v <- cbind(tree$node_id, t(tree$params_sd));
	
	if (tree$caption == "TERMINAL")
	{
		return(v);
	}
	
	r <- se.rec(tree$right_child, leafs.only, level+1);
	l <- se.rec(tree$left_child, leafs.only, level+1);
	if(leafs.only){
	  data <- rbind(l,r);
	}
	if(!leafs.only){
	  data <- rbind(v,l,r);
	}

	return(data);
	
}
