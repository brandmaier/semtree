#' Returns all estimates of a tree
#' 
#' Return model estimates of the tree.
#' 
#' 
#' @param tree A semtree object.
#' @param \dots Optional arguments.
#' @author Andreas M. Brandmaier, John J. Prindle
#' @references Brandmaier, A.M., Oertzen, T. v., McArdle, J.J., & Lindenberger,
#' U. (2013). Structural equation model trees. \emph{Psychological Methods},
#' 18(1), 71-86.
#' @export
modelEstimates <- function(tree, ...)
{
  # Compile Model estimates by Terminal Nodes. Includes Globally Constrained Elements
  
  if (tree$caption == "TERMINAL")
  { 
    parms <- omxGetParameters(tree$model, free=NA)
    parms <- rbind(parms,rep(NA,length(parms)))
    #nrow(summary(tree$model)$parameters[,c("name", "Std.Error")])
    for(i in 1:nrow(summary(tree$model)$parameters[,c("name", "Std.Error")])){
      for(j in 1:ncol(parms)){
        if(colnames(parms)[j]==summary(tree$model)$parameters[,c("name")][i]){ 
          parms[2,j] <- summary(tree$model)$parameters[,c("Std.Error")][i]
        }
      }
    }
    rownames(parms) <- c("est","se")
    
    v <- list()
    v[[1]] <- list(node=tree$node_id,Parms=parms)
    #cat(tree$caption,"\n")
    return(v)
  }
  
  r <- modelEstimates(tree$right_child);
  l <- modelEstimates(tree$left_child);
  
  data <- append(l, r)
  
  return(data)
}
