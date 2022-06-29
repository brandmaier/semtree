#' @title Return table with parameter contributions to the score-based test
#' statistics
#' @description Returns a table with the contributions of the model parameters
#' to the score-based test statistics.
#' @param tree a semtree object.
#' @param standardized logical value indicating whether standardized or raw
#' (default) contributions are to be returned. 
#' @return A matrix containing the parameter contributions to the score-based
#' test statistics. The matrix has \eqn{n} rows and \eqn{k} columns, where
#' \eqn{n} is the number of non-leaf nodes of the tree and \eqn{k} is the number
#' of model parameters. The rows are named by the node IDs as given
#' \code{getNodeById} and the columns are named as in \code{coef}. 
#' @author Manuel Arnold
#' @export

getParContrib <- function(tree, standardized = FALSE) {
  
  stopifnot("Score-guided SEM tree required" = tree$control$method == "score")
  
  n_nodes <- getNumNodes(tree)
  
  list_nodes <- lapply(seq_len(n_nodes), FUN = function(x) {
    getNodeById(tree = tree, id = x)
  })
  
  id_leafs <- sapply(list_nodes, isLeaf)
  
  split_nodes <- seq_len(n_nodes)[!id_leafs]
  
  par_contrib <- sapply(list_nodes[split_nodes], FUN = function(x) {
    x$result$par.contrib
  })
  
  par_contrib <- t(par_contrib)
  rownames(par_contrib) <- split_nodes
  
  if (standardized) {
    par_contrib <- apply(par_contrib, MARGIN = 1, FUN = function(x) {
      x / sum(x)
    })
  }
  
  par_contrib
  
}
