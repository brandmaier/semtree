#' @title Return table with standardized differences of post-split parameters
#' @description Returns a table with the standardized differences of post-split
#' parameters. Standardized differences are computed by calculating the
#' differences of the post-split nodes devided by the pooled standard errors.
#' @param tree a semtree object.
#' @return A matrix containing the standardized parameter differences. The
#' matrix has \eqn{n} rows and \eqn{k} columns, where \eqn{n} is the number of
#' non-leaf nodes of the tree and \eqn{k} is the number of model parameters. The
#' rows are named by the node IDs as given \code{getNodeById} and the columns
#' are named as in \code{coef}. 
#' @author Manuel Arnold
#' @export

getStandDiff <- function(tree) {
  
  n_nodes <- getNumNodes(tree)
  
  list_nodes <- lapply(seq_len(n_nodes), FUN = function(x) {
    getNodeById(tree = tree, id = x)
  })
  
  id_leafs <- sapply(list_nodes, isLeaf)
  
  split_nodes <- seq_len(n_nodes)[!id_leafs]
  n_split_nodes <- length(split_nodes)
  
  standDiff <- matrix(NA, nrow = n_split_nodes, ncol = length(tree$params))
  rownames(standDiff) <- split_nodes
  colnames(standDiff) <- tree$param_names
  
  for (i in 1:n_split_nodes) {
    
    left_child <- list_nodes[[split_nodes[i]]]$left_child
    right_child <- list_nodes[[split_nodes[i]]]$right_child
    
    standDiff[i, ] <- matrix((left_child$params - right_child$params)^2 /
      (left_child$params_sd^2 + right_child$params_sd^2),
      nrow = 1, ncol = length(tree$params))
    
  }
  
  standDiff
  
}
