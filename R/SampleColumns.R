#
# samples a number of columns (specified by "mtry") from a
# set of columns in "dataset" specified by the names given in "covariates"
# 
#

sampleColumns <- function(dataset, covariates, mtry) {
		 cov.ids <- which(names(dataset) %in% covariates)
		 # sample length(cov.ids)-mtry of those to exclude
		 if ( length(cov.ids)-mtry > 0) {
		 rem.ids <- sample(cov.ids, length(cov.ids)-mtry)
		 dataset <- dataset[, -rem.ids]
		 } else {
		 	# don't act and keep all
		 }
	
		 return(dataset)
	 }
		 