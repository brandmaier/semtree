#' @exportS3Method 
logLik.semtree <- function(object, ...)
{
	return(logLik(object$model, ...))
}