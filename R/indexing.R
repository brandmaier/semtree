#' @exportS3Method "[" semtree
`[.semtree` <- function(x, i, ...) 
{
	getNodeById(x, i)
}

#' @exportS3Method "[" semforest
`[.semforest` <- function(x, i, ...) 
{
  x$forest[[i]]
}
