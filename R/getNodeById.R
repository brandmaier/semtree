#' Get Node By Id
#' 
#' Return a node matching a given node ID
#' 
#' 
#' @param tree A SEM Tree object.
#' @param id Numeric. A Node id.
#' @author Andreas M. Brandmaier, John J. Prindle
#' @references Brandmaier, A.M., Oertzen, T. v., McArdle, J.J., & Lindenberger,
#' U. (2013). Structural equation model trees. \emph{Psychological Methods},
#' 18(1), 71-86.
#' @export
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
