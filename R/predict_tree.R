evaluateTreePrediction <- function(tree, test_set, leaf_ids=NULL)
{
  
  if (is.null(tree)) return(NULL)
  if (is.null(test_set)) return(NULL)
  #model_ids <- tree$model.ids;
  
  mapping <- getIdParameterMapping(tree)
  
  # get a mapping of dataset rows to leaf ids
  if (is.null(leaf_ids)) {
    leaf_ids <- traverse(tree, test_set)
  }
  
  # for each leaf, calculate deviance of each data row
  error <- 0
  cnt <- 0
  #leafwise <- rep(NA, length(unique(leaf_ids)))
  #dev <- 0
  for (leaf_id in unique(leaf_ids))
  {
    #print(leaf_id)
    temp_set <- test_set[leaf_ids==leaf_id, ];
    
    #params <- mapping[ mapping[,1]==leaf_id, 2:ncol(mapping)]
    #model <- set_model_parameters(tree$model, params)
    
    leaf <- getNodeById( tree, leaf_id)
    model <- leaf$model
    #add_dev <- evaluateDataLikelihood(leaf$model, temp_set[,,drop=F], data_type )
    
    # get model mean
    templen <- dim(temp_set)[1]
   
    imina <- solve( diag(rep(1,dim(model@matrices$A@values)[1]))- model@matrices$A@values )
    ram.mean <- model@matrices$M@values %*% imina %*% t(model@matrices$F@values)
    for (j in 1:templen)
    {
      row.value <- temp_set[j,model@manifestVars]
      localerror <- sum(row.value-ram.mean, na.rm=T)^2
      error <- error + localerror
      cnt <- cnt + length(row.value)-is.na(row.value)
     # cat(row.value," ",ram.mean," Err: ",localerror,"  ",temp_set$x[j],"row#",rownames(temp_set)[j],"\n")
    }
    #cat(paste("Add deviance ",add_dev,"leaf",leaf$node_id,"\n")); ##, toString(which(leaf_ids==leaf_id)) ,"\n"))
#    dev <- dev + add_dev;
  }
  
  result <- list()
  result$count <- cnt
  result$mse <- error/cnt
  result$rmse <- sqrt(result$mse)
  result$num_models <- length(unique(leaf_ids))
  result$deviance <- result$mse
  
  return(result);
  
}

