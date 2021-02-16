#' Evaluate Tree -2LL
#' 
#' A helper function to evaluate the negative two log-likelihood (-2LL) of leaf (terminal) nodes for a
#' dataset. When given a \code{\link{semtree}} and a unique dataset, the model
#' estimates -2LL for the tree parameters and data subsets that fit the tree
#' branching criteria.
#' 
#' 
#' @param tree A fitted \code{\link{semtree}} object
#' @param test_set Dataset to fit to a fitted \code{\link{semtree}} object
#' @param data_type type of data ("raw", "cov", "cor")
#' @param leaf_ids Identifies which nodes are leaf nodes. Default is NULL,
#' which checks model for leaf nodes and fills this information in
#' automatically.
#' @return A list with two elements: \item{deviance}{Combined -2LL for leaf
#' node models of the tree.} \item{num_models}{Number of leaf nodes used for
#' the deviance calculations.} %% ...
#' @author Andreas M. Brandmaier, John J. Prindle
#' @seealso \code{\link{evaluateDataLikelihood}}, \code{\link{semtree}},
#' \code{\link{semforest}}
#' @references Brandmaier, A.M., Oertzen, T. v., McArdle, J.J., & Lindenberger,
#' U. (2013). Structural equation model trees. \emph{Psychological Methods},
#' 18(1), 71-86.
#' @export
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
