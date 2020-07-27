#
#
# This function takes a tree and a dataset (both original and permuted) and
# computes the difference in log-likelihoods considering focus parameters
#
evaluateTreeFocus <-
  function(tree, test_set, data_type="raw", leaf_ids=NULL)
  {
    
    # get a mapping of dataset rows to leaf ids
    if (is.null(leaf_ids)) {
      leaf_ids <- traverse(tree, test_set)
    }
    
    # for each leaf, calculate deviance of each data row
    dev <- 0
    for (leaf_id in unique(leaf_ids))
    {
      # get all data rows from current leaf
      temp_set <- test_set[leaf_ids==leaf_id, ];
      
      # get the leaf object
      leaf <- getNodeById( tree, leaf_id)
      
      # test if node has a focus model
      if (is.null(leaf$focus.model)) {
        ui_warn("No focus model available!")
        return(NA)
      }
      
      # evaluate log-likelihood from baseline and focus model
      #baseline = evaluateDataLikelihood(leaf$model, temp_set[,,drop=F], data_type )
      ll.focus = evaluateDataLikelihood(leaf$focus.model, 
                                              temp_set[,,drop=F], data_type )
      
      # evaluate log-likelihood after permutation
      
      
      # add up log-likelihoods
      dev <- dev + ll.focus
    }
    
    result <- list()
    result$deviance <- dev
    result$num_models <- length(unique(leaf_ids))
    
    return(result);
    
  }

# TODO: finish this block
# TODO: remove earlier computation of baseline ll
# compute influence of focus parameter before permutation
#ll.baseline <- eval.fun(tree, oob.data)$deviance
#ll.baseline <- fitSubmodels(tree$model, subset1, subset2, 
#                            control, invariance=constraints$focus.parameters)
# compute misfit of focus parameter after permutation
#ll.permuted <- eval.fun(tree, oob.data.permuted)$deviance
#ll.diff <- -ll.baseline + ll.permuted
#ui_warn("Unfinished implementation!")


#}