isLeaf <- function(tree)
{
	if (!inherits(tree,"semtree")) return(NULL)
	return(tree$caption=="TERMINAL")
}
