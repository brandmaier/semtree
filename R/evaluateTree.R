#'
#' Compute the Likelihood of some data given a tree
#'
#' @param tree semtree. A SEM tree to be evaluated.
#' @param test_set data.frame The data to be evaluated under the tree
#'
#' @returns Numeric Returns the -2 log likelihood of the data given the tree
#'
evaluateTree <-
  function(tree,
           test_set,
           data_type = "raw",
           leaf_ids = NULL)
  {
    # get a mapping of dataset rows to leaf ids
    if (is.null(leaf_ids)) {
      leaf_ids <- traverse(tree, test_set)
    }
    
    # for each leaf, calculate deviance of each data row
    dev <- 0
    for (leaf_id in unique(leaf_ids))
    {
      temp_set <- test_set[leaf_ids == leaf_id,]
      
      
      leaf <- getNodeById(tree, leaf_id)
      
      # add up log-likelihoods
      dev <-
        dev + evaluateDataLikelihood(leaf$model, temp_set[, , drop = F], data_type)
    }
    
    result <- list()
    result$deviance <- dev
    result$num_models <- length(unique(leaf_ids))
    
    return(result)
    
    
  }
