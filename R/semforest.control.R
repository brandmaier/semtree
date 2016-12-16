semforest.control <- function(num.trees=5, sampling="subsample", control=NA, mtry=2)
{
	options <- list()
	options$num.trees <- num.trees
	options$sampling <- sampling
	options$premtry <- 0
	options$mtry <- mtry
	if (is.na(control)) {
		options$semtree.control <- semtree.control()
    options$semtree.control$method <- "fair"
    options$semtree.control$alpha <- 1
	} else {
		options$semtree.control <- control
	}
	class(options) <- "semforest.control"
    return(options)
}