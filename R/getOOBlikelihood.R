oobapply <- function(tree, data) {
	return(evaluateTree(tree,data$oob.data)$deviance)
}

getOOBlikelihood<-function(forest)
{
	#evaluate_tree(tree, forest$forest.data[[4]]$oob.data)$deviance
	dat <- mapply(FUN=oobapply, forest$forest, forest$forest.data, SIMPLIFY=TRUE)
}

