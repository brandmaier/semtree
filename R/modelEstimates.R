modelEstimates <- function(tree, level=0, ...)
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
  
  #level = 0
  r <- modelEstimates(tree$right_child, level+1);
  l <- modelEstimates(tree$left_child, level+1);
  
  data <- append(l, r)
  
  return(data)
}