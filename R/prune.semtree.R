#' @exportS3Method prune semtree
prune.semtree <- function(object, max.depth=NULL, converged=FALSE, ...)
{
	if (is.null(max.depth) && isFALSE(converged)) { return(object);}
	
	return(pruneSemtreeRec(tree=object,max.depth=max.depth, cur.depth=0, converged=converged))
}

pruneSemtreeRec <- function(tree, max.depth, cur.depth, converged)
{
	
	if (tree$caption != "TERMINAL") {
	  
	  termination <- FALSE
	  if (!is.null(max.depth))	termination <- cur.depth >= max.depth
	  if (isTRUE(converged)) {
	    not_converged <-  (!hasConverged(tree$model)) | (!hasConverged(tree$left_child$model)) | (!hasConverged(tree$right_child$model))
	    termination <- termination | not_converged
	  }
		
		if (!termination) {
			
			tree$left_child <- pruneSemtreeRec(tree$left_child, max.depth, cur.depth+1, converged)
			tree$right_child <- pruneSemtreeRec(tree$right_child, max.depth, cur.depth+1, converged)
			
		} else {
			tree$caption = "TERMINAL";
			tree$left_child <- NULL;
			tree$right_child <- NULL;
		}
		
	}
  
  tree$traverse.fun <- NULL
  tree$traverseRow.fun <- NULL
  
	
	return(tree);
}