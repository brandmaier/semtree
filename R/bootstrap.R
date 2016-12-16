bootstrap <- function(dataset, mtry=2, covariates=NULL, return.oob=F, type="bootstrap") {

		if (mtry > 0) {
		 #cov.ids <- which(names(dataset) %in% covariates)
		 # sample length(cov.ids)-mtry of those to exclude
		 #rem.ids <- sample(cov.ids, length(cov.ids)-mtry)
		 #dataset <- dataset[, -rem.ids]
		 dataset <- sampleColumns(dataset, covariates, mtry)
		}	

		N <- dim(dataset)[1]
    
    if (type=="bootstrap") {
		  indices <- sample(1:N,N,replace=T)
    } else if (type=="subsample") {
      indices <- sample(1:N,round(0.632*N),replace=F)
    } else {
      stop("Error! Unknown resampling procedure!")
    }
		  bootstrap.data <- dataset[indices,]
		  oob.indices <- setdiff( 1:N, unique(indices))
		  oob.data <- dataset[oob.indices, ]

	
		if (return.oob) {
			return(list(bootstrap.data = bootstrap.data, oob.data = oob.data))
		} else {
			return(bootstrap.data)
		}
	
		
}