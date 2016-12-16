calculateBestSubsets <- function(model, mydata, sub1, sub2, result)
{
	#message(paste(result,collapse="\n"))
	missingSet <- mydata[is.na(mydata[,result$col.max]),]
	model1 <- OpenMx::mxModel(model, OpenMx::mxData(sub1, type="raw"))
	model2 <- OpenMx::mxModel(model, OpenMx::mxData(sub2, type="raw"))
	values1 <- OpenMx::omxGetParameters(OpenMx::mxRun(model1, silent=T, useOptimizer=T, suppressWarnings=T))
	values2 <- OpenMx::omxGetParameters(OpenMx::mxRun(model2, silent=T, useOptimizer=T, suppressWarnings=T))
	model1 <- OpenMx::omxSetParameters(model1, labels=names(OpenMx::omxGetParameters(model)),free=FALSE, values=values1 )
	model2 <- OpenMx::omxSetParameters(model2, labels=names(OpenMx::omxGetParameters(model)),free=FALSE, values=values2 )
  compareLL <- c()
	for(i in 1:nrow(missingSet)){
	  model1 <- OpenMx::mxModel(model1, OpenMx::mxData(missingSet[i,], type="raw"))
    LL1 <- safeRunAndEvaluate(model1, return.model=T)$LL
	  model2 <- OpenMx::mxModel(model2, mxData(missingSet[i,], type="raw"))
    LL2 <- safeRunAndEvaluate(model2, return.model=T)$LL
    compareLL <- c(compareLL, (LL1-LL2))
    if(compareLL[i]>0){sub1<- rbind(sub1,missingSet[i,])}
    else{sub2<- rbind(sub2,missingSet[i,])}
	}
  
  missingSet <- data.frame(missingSet, compareLL)
	
	return(list(sub1=sub1,sub2=sub2))
}