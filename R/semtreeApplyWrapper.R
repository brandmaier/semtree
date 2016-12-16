
semtreeApplyWrapper <- function(data, seed, skip,
                                model, semtree.control, with.error.handler=TRUE, covariates, constraints, ...)
{
 # browser()
	if(!is.na(seed)) {
    	cat("Set seed ",seed," for tree in forest\n")
		set.seed(seed)
	}
  
	if (skip) return(NULL);
  
#	browser()
	result <- NULL
	
	if (with.error.handler) {
	
	tryCatch({

		result <- semtree(model=model,dataset=data$bootstrap.data,
                      control=semtree.control, predictors=covariates, constraints=constraints, ...)
		
		}, error=function(err) {
			errmsg <- paste(date(),paste(err), paste(traceback()),sep="\n")
			
			write(errmsg, file="error.log",append=TRUE)	
			return(NULL)
		})
		
	} else {
		
		result <- semtree(model=model,dataset=data$bootstrap.data,control=semtree.control, 
                      predictors=covariates, constraints=constraints, ...)
	}
	
	return(result)
}
