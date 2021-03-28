#' @exportS3Method prune semtree
prune.semtree <- function(object, max.depth=NULL, ...)
{
	if (is.null(max.depth)) { return(object);}
	
	return(pruneSemtreeRec(object,max.depth, 0))
}

pruneSemtreeRec <- function(tree, max.depth, cur.depth)
{
	
	if (tree$caption != "TERMINAL") {
		
		if (cur.depth < max.depth) {
			
			tree$left_child <- pruneSemtreeRec(tree$left_child, max.depth, cur.depth+1)
			tree$right_child <- pruneSemtreeRec(tree$right_child, max.depth, cur.depth+1)
			
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