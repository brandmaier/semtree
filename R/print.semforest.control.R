print.semforest.control <- function(x, ...)
{
	stopifnot(is(x,"semforest.control"))

	cat("SEM-Forest control:\n-----------------\n")

	cat("Number of Trees: ", x$num.trees,"\n")
	cat("Sampling: ", x$sampling,"\n");
	cat("Comparisons per Node:", x$mtry,"\n\n")
	print.semtree.control(x$semtree.control)
}
