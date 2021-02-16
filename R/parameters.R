#' SEMtrees Parameter Estimates Table
#' 
#' Returns a table of parameters with columns corresponding to freely estimated
#' parameters and rows corresponding to nodes in the tree.
#' 
#' The row names of the resulting data frame correspond to internal node ids
#' and the column names correspond to parameters in the SEM. Standard errors of
#' the estimates can be obtained from \code{\link{parameters}}.
#' 
#' @param tree A SEMtree object obtained from \code{\link{semtree}}
#' @param leafs.only Default = TRUE. Only the terminal nodes (leafs) are
#' printed. If set to FALSE, all node parameters are written to the
#' \code{data.frame}.
#' @return Returns a \code{data.frame} with rows for parameters and columns for
#' terminal nodes.
#' @author Andreas M. Brandmaier, John J. Prindle
#' @seealso \code{\link{semtree}}, \code{\link{semtree.control}},
#' \code{\link{se}}
#' @references Brandmaier, A.M., Oertzen, T. v., McArdle, J.J., & Lindenberger,
#' U. (2013). Structural equation model trees. \emph{Psychological Methods},
#' 18(1), 71-86.
#' @export
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
