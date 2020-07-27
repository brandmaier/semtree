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
    dev <- 0
    for (leaf_id in unique(leaf_ids))
    {
      # obtain all cases from a specific leaf
      temp_set <- test_set1[leaf_ids==leaf_id, ];
      
      # get leaf node by id
      leaf <- getNodeById( tree, leaf_id)
      
      # add up log-likelihoods
      dev <- dev + evaluateDataLikelihood(leaf$model, temp_set[,,drop=F], data_type )
    }
    
    result <- list()
    result$deviance <- dev
    result$num_models <- length(unique(leaf_ids))
    
    return(result);
    
  }
