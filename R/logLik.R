#' @exportS3Method logLik semtree
logLik.semtree <- function(object, ...)
{
	return(logLik(object$model, ...))
}