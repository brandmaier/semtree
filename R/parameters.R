parameters <- function(tree, leafs.only=TRUE) {
	
	data <- parameters.rec(tree, leafs.only, 0)

	if (nrow(data) > 1) {
	 data <- round(data.frame(data[,-1], row.names=data[,1]),digits=3)
	 names(data) <- tree$param_names;
	 data <- t(data)
	} else {
	  data <- round(data.frame(data[,-1]),digits=3)
	}
	

   
	

	return(data);
}


parameters.rec <- function(tree, leafs.only=TRUE, level=0)
{
	v <- cbind(tree$node_id, t(tree$params));
	
	if (tree$caption == "TERMINAL")
	{
		return(v);
	}
	
	l <- parameters.rec(tree$left_child, leafs.only, level+1);
	r <- parameters.rec(tree$right_child, leafs.only, level+1);
  
  if(leafs.only){
	  data <- rbind(l,r);
  }
	if(!leafs.only){
	  data <- rbind(v,l,r);
	}

	return(data);
	
}

# parameters(result2, leafs.only=TRUE)
# print(result2)
# print.semtree
# print(result2)
# plot(subtree(result2,startNode=2))
