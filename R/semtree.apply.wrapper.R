#semtree.apply.wrapper <- function(data, model, semtree.control, ...)
semtree.apply.wrapper <- function(data, seed, model, semtree.control, with.error.handler=TRUE, covariates, ...)
{

	if(!is.na(seed)) {
    	cat("Set seed ",seed," for tree in forest\n")
		set.seed(seed)
	}

	result <- NULL
	
	if (with.error.handler) {
	
	tryCatch({

		result <- semtree(model=model,dataset=data$bootstrap.data,
                      control=semtree.control, covariates=covariates, ...)
		
		}, error=function(err) {
			errmsg <- paste(date(),paste(err), paste(traceback()),sep="\n")
			
			write(errmsg, file="error.log",append=TRUE)	
			return(NULL)
		})
		
	} else {
		
		result <- semtree(model=model,dataset=data$bootstrap.data,control=semtree.control, 
                      covariates=covariates,...)
		
		ui_ok("Hi!")
		browser()
	}
	
	return(result)
}
