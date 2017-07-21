evaluateTreeConditional <-
  function(tree, test_set_list, data_type="raw", leaf_ids=NULL)
  {
    test_set1 <- test_set_list[[1]]
    
    mapping <- getIdParameterMapping(tree)
    
    # get a mapping of dataset rows to leaf ids
    if (is.null(leaf_ids)) {
      leaf_ids <- traverseConditional(tree, test_set_list[[1]], test_set_list[[2]])
    }
    
    # for each leaf, calculate deviance of each data row
    
    #leafwise <- rep(NA, length(unique(leaf_ids)))
    dev <- 0
    for (leaf_id in unique(leaf_ids))
    {
      # obtain all cases from a specific lead
      temp_set <- test_set1[leaf_ids==leaf_id, ];
      
      # get lead node by id
      leaf <- getNodeById( tree, leaf_id)
      
      add_dev <- evaluateDataLikelihood(leaf$model, temp_set[,,drop=F], data_type )
      #cat(paste("Add deviance ",add_dev,"leaf",leaf$node_id,"\n")); ##, toString(which(leaf_ids==leaf_id)) ,"\n"))
      dev <- dev + add_dev;
    }
    
    result <- list()
    result$deviance <- dev
    result$num_models <- length(unique(leaf_ids))
    
    return(result);
    
  }
