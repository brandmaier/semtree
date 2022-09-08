#' @title Return table with parameter differences of a tree
#' @description Returns a table with some measure of parameter
#' differences between post-split nodes.
#' @param tree a semtree object.
#' @param measure a character. "wald" (default) gives the squared parameter
#' differences devided by their pooled standard errors. "test" gives the
#' contributions of the parameters to the test statistic."raw" gives the
#' absolute values of the parameter differences.
#' @param normalize logical value; if TRUE parameter differences of each split
#' are divided by sum of all differences the corresponding split. Set to FALSE
#' by default.
#' @return A matrix containing parameter differences. The
#' matrix has \eqn{n} rows and \eqn{k} columns, where \eqn{n} is the number of
#' non-leaf nodes of the tree and \eqn{k} is the number of model parameters. The
#' rows are named by the node IDs as given \code{getNodeById} and the columns
#' are named as in \code{coef}. 
#' @author Manuel Arnold
#' @export

getParDiffTree <- function(tree, measure = "wald", normalize = FALSE) {
  
  if (measure == "test" & !tree$control$method == "score") {
    stop("Contributions to test statistics are limited to score-guided SEM trees.")
  }
  
  n_nodes <- getNumNodes(tree)
  
  list_nodes <- lapply(seq_len(n_nodes), FUN = function(x) {
    getNodeById(tree = tree, id = x)
  })
  
  id_leafs <- sapply(list_nodes, isLeaf)
  
  split_nodes <- seq_len(n_nodes)[!id_leafs]
  
  if (measure %in% c("raw", "wald")) {
    
    n_split_nodes <- length(split_nodes)
    
    ParDiff <- matrix(NA, nrow = n_split_nodes, ncol = length(tree$params))
    rownames(ParDiff) <- split_nodes
    colnames(ParDiff) <- tree$param_names
    
    for (i in 1:n_split_nodes) {
      
      left_child <- list_nodes[[split_nodes[i]]]$left_child
      right_child <- list_nodes[[split_nodes[i]]]$right_child
      
      ParDiff[i, ] <- abs(left_child$params - right_child$params)
      
      if (measure == "wald") {
        ParDiff[i, ] <- ParDiff[i, ]^2 / (left_child$params_sd^2 +
                                            right_child$params_sd^2)
      }
    }
  }
  
  if (measure == "test") {
    
    ParDiff <- sapply(list_nodes[split_nodes], FUN = function(x) {
      x$result$par.contrib
    })
    
    ParDiff <- t(ParDiff)
    rownames(ParDiff) <- split_nodes
    
  }
  
  if (normalize) {
    ParDiff <- t(apply(ParDiff, MARGIN = 1, FUN = function(x) {
      x / sum(x)
    }))
  }
  
  ParDiff
  
}
