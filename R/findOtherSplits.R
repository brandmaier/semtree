#' Find Other Node Split Values
#' 
#' Search tool to search nodes for alternative splitting values found during
#' the \code{\link{semtree}} process. Given a particular node, competing split
#' values are listed assuming they also meet the criteria for a significant
#' splitting value as set by \code{\link{semtree.control}}.
#' 
#' 
#' @param node A node from a \code{\link{semtree}} object.
#' @param tree A \code{\link{semtree}} object which the node is part of.
#' @return A \code{data.frame()} with rows corresponding to the variable names
#' and split values for alternative splits found in the node of interest. %%
#' ...
#' @author Andreas M. Brandmaier, John J. Prindle
#' @references Brandmaier, A.M., Oertzen, T. v., McArdle, J.J., & Lindenberger,
#' U. (2013). Structural equation model trees. \emph{Psychological Methods},
#' 18(1), 71-86.
findOtherSplits <-
function(node, tree)
{
	if (tree$p.values.valid) {
		idx <- which(node$p_values <= tree$options$alpha)
	} else {
		idx <- which(node$lr_values > 0)
	}
	
	idx2 <- node$covariate.ids[idx]
	
	cvalues <- as.numeric( round( node$lr_values[idx], 3) )
	cnames <- names(tree$recoding$dataset)[idx2]
	
	#cat(cvalues)
	#cat(cnames)
	#print(cvalues)
	
	df<-data.frame( cbind(cnames,  cvalues ) )
	df<-data.frame( cnames, cvalues  )
	
	#df <- sort(df, by =~ df[,2])
	df <- df[with(df,order(-df[,2])) ,]
	
	return(df)
}
