relative.misfit <- function(node1, node2, scaled=T)
{
	param.names <- names(omxGetParameters(node1$model))
	
	flist <- rep(NA, length(param.names))
	
	subset1 <- node1$model@data@observed
	subset2 <- node2$model@data@observed
		
	basesum <- fitSubmodels(node1$model, subset1, subset2)
	
	i<-1
	for (param in param.names) {
		
		
	
		fsum <- fitSubmodels(node1$model, subset1, subset2, invariance=param)
		flist[i] <- -basesum+fsum
		i<-i+1
	}
	
	if (scaled) flist <- flist / sum(flist)
	
	names(flist) <- param.names
	
	return(flist)
}



#result <- relative.misfit( tree$left_child, tree$right_child)