#' @exportS3Method print semtree
print.semtree <-
function( x, level=0, p.values.valid=NULL, ... )
{
	tree <- x	

	indent <- paste(rep("|   ",level),collapse="",sep="")
	
	if (level > 0) { edge_label <- tree$edge_label } else { edge_label <- "ROOT"; }
	
	caption <- tree$caption;
	if (caption == "TERMINAL")
	{
		#caption <- paste(caption,"(ID=",tree$node_id,")",sep="")	
		output <- paste(indent,"|-[",tree$node_id,"] ",caption," [N=",tree$N,"]\n",sep="");
	}

  else {
		output <- paste(indent,"|-[",tree$node_id,"] ",caption," [N=",tree$N," ","LR=",round(tree$lr,digits=2),", df=",tree$df,"]\n",sep="");
	}
	
	if (tree$caption != "TERMINAL")
	{	
		output <-paste(output,print.semtree( tree$left_child, level+1, p.values.valid),
		               print.semtree( tree$right_child, level+1, p.values.valid));
	}	
	
	if (level==0) {
		output <- paste("SEMtree with numbered nodes\n",output); 
		cat(output);	
	} else {
		return(output);	
	}
}

#' @exportS3Method print semtree
print.semtree_stripped <- function(x) {
  cat("SEM tree [stripped]\n")
  print.semtree(x)
}
