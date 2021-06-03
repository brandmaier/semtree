#' SEM Forest Case Proximity
#' 
#' A \code{\link{semforest}} process to represent proportion of trees where
#' each case are in the same leaf nodes. The values are bounded (0,1), where
#' higher values are closer in proximity.
#' 
#' 
#' @aliases proximity plot.proximity plot.semforest.proximity
#' @param forest A \code{\link{semforest}} object.
#' @param dataset A dataset to compute proximity values for.
#' @param type Missingness accounted for. (0 = no, 1 = yes)
#' @param aggregate Boolean marker to compute aggregate proximity scores.
#' @param \dots Optional arguments.
#' @return A matrix with dimensions NxN is returned. The values of each cell
#' are bounded (0,1) and represent proportion of trees where each case are in
#' the same leaf nodes.
#' @author Andreas M. Brandmaier, John J. Prindle
#' @seealso \code{\link{semforest}}, \code{\link{semtree}}
#' @references Brandmaier, A.M., Oertzen, T. v., McArdle, J.J., & Lindenberger,
#' U. (2013). Structural equation model trees. \emph{Psychological Methods},
#' 18(1), 71-86.
#' @export
proximity <- function(forest, dataset=NULL, type=0, aggregate=T, ...)
{
  if (is.null(dataset)) {
    dataset <- forest$data
  }
  
	N <- dim(dataset)[1]
	prox.matrix <- matrix(rep(0,N*N),nrow=N,ncol=N)
	
	K <- length(forest$forest)
  
  
  if (type == 0) {
    prox.fun <- proximity.tree.matrix
  } else {
    prox.fun <- missingness.proximity.tree.matrix
  }
	
  #quasi map-reduce-step here:
  bool.matrix <- future.apply::future_lapply(forest$forest, FUN=prox.fun, dataset, future.seed=TRUE)
  
  for (i in 1:K)
	{
		prox.matrix <- prox.matrix + bool.matrix[[i]]
	}	
	
  #if(normalize)
	prox.matrix <- prox.matrix / K
  
 # diag(prox.matrix) <- 
	
  class(prox.matrix) <- "semforest.proximity"
  
  if (aggregate)
    return(prox.matrix)
  else
    return(bool.matrix)  #-- good for debugging, return list of bool matrices
	
}

proximity.tree.matrix <- function(tree, dataset)
{
		N <- dim(dataset)[1]

		tv <- as.vector(traverse(tree,dataset))
		bool.matrix <- matrix(rep(0,N*N),nrow=N,ncol=N)
		for (j in 1:length(tv)) {
			for (k in 1:length(tv)) {
				bool.matrix[j,k] <- ifelse(tv[j]==tv[k],1,0)
			}
		}
		return(bool.matrix)
}

missingness.proximity.tree.matrix <- function(tree, dataset)
{
  N <- dim(dataset)[1]
  
  # return constant matrix for empty tree
  if (getNumNodes(tree)==1) {
    return(matrix(1,nrow=N,ncol=N))
  }
  
  cmap <- get.children.map(tree)
  for (i in 1:length(cmap)) {
    cmap[[i]] <- c(cmap[[i]],i)
  }
  
  tv <- as.vector(traverse(tree,dataset))
  bool.matrix <- matrix(rep(0,N*N),nrow=N,ncol=N)
  for (j in 1:length(tv)) {
    for (k in 1:length(tv)) {
      bool.matrix[j,k] <- ifelse( any(cmap[[tv[j]]] %in% cmap[[tv[k]]]),1,0)
    }
  }
  return(bool.matrix)
}
