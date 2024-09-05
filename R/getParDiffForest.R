#' @title Return list with parameter differences of a forest
#' @description Returns a list of tables with some measure of parameter
#' differences between post-split nodes.
#' @param forest a semforest object.
#' @param measure a character. "wald" (default) gives the squared parameter
#' differences divided by their pooled standard errors. test" gives the
#' contributions of the parameters to the test statistic. "raw" gives the
#' absolute values of the parameter differences.  
#' @param normalize logical value; if TRUE parameter differences of each split
#' are divided by sum of all differences the corresponding split. Set to FALSE
#' by default.
#' @return A list with data.frames containing parameter differences for each
#' tree of the forest. The rows of the data.frames correspond to the non-leaf
#' nodes of the respective trees. The first column contains the name of the
#' predictor variables and the remaining columns contain the parameter
#' differences. The rows of the data.frames are named by the node IDs as given
#' \code{getNodeById} and the columns are named as in \code{coef}. 
#' @author Manuel Arnold
#' @export

getParDiffForest <- function(forest, measure = "wald", normalize = FALSE) {
  
  if (measure == "test" & !forest$control$semtree.control$method == "score") {
    stop("Contributions to test statistics are limited to score-guided SEM trees.")
  }
  
  n_trees <- length(forest$forest)
  res <- vector(mode = "list", length = n_trees)
  
  for (i in 1:n_trees) {
    
    n_nodes <- getNumNodes(forest$forest[[i]])
    
    if (n_nodes == 1) next
    
    # Get parameter differences
    par_diff <- getParDiffTree(tree = forest$forest[[i]],
                               measure = measure,
                               normalize = normalize)
    
    list_nodes <- lapply(seq_len(n_nodes), FUN = function(x) {
      getNodeById(tree = forest$forest[[i]], id = x)
    })
    
    # Get predictor names
    id_leafs <- sapply(list_nodes, isLeaf)
    
    split_nodes <- seq_len(n_nodes)[!id_leafs]
    
    predictor_names <- sapply(list_nodes[split_nodes], FUN = function(x) {
      x$rule$name})
    
    par_diff <- data.frame(predictor = predictor_names, par_diff)
    
    res[[i]] <- par_diff
    
  }
  
  res
  
}
  