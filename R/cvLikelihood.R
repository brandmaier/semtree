# currently not in use
#
#
cvLikelihood <- function( model, subset1, subset2, fold_association1,fold_association2, control, invariance=NULL )
{
	folds <- length(unique(c(fold_association1, fold_association2)))	

	lrs <- rep(NA, folds)
  	for (fold in 1:folds)
  	{
  		
 	 	training_subset1 <- subset1[fold_association1!=fold,,drop=F] # 4 out of 5 folds
		test_subset1 <- subset1[fold_association1==fold,,drop=F] # 1 out of 5 folds 	 		
  		
  		if (!is.null(subset2)) {
  		  training_subset2 <- subset2[fold_association2!=fold,,drop=F] # 4 out of 5 folds
  		  test_subset2 <- subset2[fold_association2==fold,,drop=F] # 1 out of 5 folds
  		  
  		  result <- fitSubmodels(model, training_subset1, training_subset2, control, invariance,return.models=TRUE)		
  		  if(!is.list(result)){return(NA)}
       
        ll1 <- evaluateDataLikelihood(result$model1, test_subset1) 
  		  ll2 <- evaluateDataLikelihood(result$model2, test_subset2)  			
      } 
      else {

        model1 <- mxAddNewModelData(model=model, data=training_subset1, name=paste("MODEL",fold,sep=" "))
        
			  out1 <- safeRunAndEvaluate(model1,return.model=T)
        ll1 <- evaluateDataLikelihood(out1$model, test_subset1) 			
  	    ll2 <- 0	
  		}
  		
		
		lrs[fold] <- (ll1+ll2)
  	}
  	return(mean(lrs,na.rm=T))

}

