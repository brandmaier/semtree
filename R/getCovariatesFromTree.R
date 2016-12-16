getCovariatesFromTree<-function(tree) {
	if (tree$caption=="TERMINAL") {
		return();
	} else {
		l <- getCovariatesFromTree(tree$left_child)
		r <- getCovariatesFromTree(tree$right_child)
		return(unique(c(l,r,tree$rule$name)))
	}
}