lgcm.forest.evaluate.prediction <- function(forest, dataset) 
{

  # get forest predictions
  all.predictions <- lapply(forest$forest, FUN=lgcm.tree.predict, dataset)
  #for (i in 1:1000) {
  #  print(i)
  #  lgcm.tree.predict(forest$forest[[i]], dataset)
#  }#

  # average predictions (without weighting them)
  avg <- Reduce("+",all.predictions)
  avg <- avg / length(forest$forest)
  
  # take first tree
  tree <- forest$forest[[1]]
  
  manifests <- tree$model@manifestVars
  
  for (i in 1:dim(dataset)[1])  # iterate through rows
  {
    obs <- dataset[i, tree$model@manifestVars]
    err <- err + sum((avg-obs)**2)
  }
  
  err <- err / (dim(dataset)[1] * length(tree$model@manifestVars) )
  
}


lgcm.tree.predict <- function(tree, dataset)
{
  trav <- traverse(tree, dataset)
  
  err <- 0
  mean.pred <- matrix(NA, nrow=dim(dataset)[1] , ncol=length(tree$model@manifestVars))
  for (i in 1:dim(dataset)[1])  # iterate through rows
  {
      node <- getNodeById(tree, trav[i])
      mean.pred[i,] <- node$model$M@values %*% t(node$model$A@values) %*% t(node$model$F@values)
  }  
  
  return(mean.pred)

}