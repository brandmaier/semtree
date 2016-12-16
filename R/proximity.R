proximity <- function(forest, dataset=NULL, type=0, aggregate=T, cluster=NULL, ...)
{
  if ("snowfall" %in% list(...)) {
    warning("Use of snowfall is deprecated and must be replaced with cluster argument from package 'parallel'! See manual")
  }
  
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
  if (is.null(cluster)) 
	  bool.matrix <- lapply(forest$forest, FUN=prox.fun, dataset)
  else
    bool.matrix <- parLapply(cl=cluster,forest$forest, FUN=prox.fun, dataset)
  
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