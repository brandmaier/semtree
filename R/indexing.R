`[.semtree` <- function(x, i, ...) 
{
	getNodeById(x, i)
}


`[.semforest` <- function(x, i, ...) 
{
  x$forest[[i]]
}
