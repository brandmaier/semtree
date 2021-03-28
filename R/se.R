
#' SEMtrees Parameter Estimates Standard Error Table
#' 
#' Returns a table of standard errors with columns corresponding to freely
#' estimated standard errors and rows corresponding to nodes in the tree.
#' 
#' The row names of the resulting data frame correspond to internal node ids
#' and the column names correspond to standard errors in the SEM. Parameter
#' estimates can be obtained from \code{\link{parameters}}.
#' 
#' @aliases se
#' @param tree A SEMtree object obtained from \code{\link{semtree}}
#' @param leafs.only Default = TRUE. Only the terminal nodes (leafs) are
#' printed. If set to FALSE, all node standard errors are written to the
#' \code{data.frame}.
#' @return Returns a \code{data.frame} with rows for parameters and columns for
#' terminal nodes.
#' @author Andreas M. Brandmaier, John J. Prindle
#' @seealso \code{\link{semtree}}, \code{\link{semtree.control}},
#' \code{\link{parameters}}
#' @references Brandmaier, A.M., Oertzen, T. v., McArdle, J.J., & Lindenberger,
#' U. (2013). Structural equation model trees. \emph{Psychological Methods},
#' 18(1), 71-86.
#' @export
se <- function(tree, leafs.only = TRUE) {
  
  data <- se.rec(tree, leafs.only, 0)
  data <-
    round(data.frame(data[,-1], row.names = data[, 1]), digits =
            3)
  
  names(data) <- tree$param_names
  data <- t(data)
  return(data)
  
}

se.rec <- function(tree,
                   leafs.only = TRUE,
                   level = 0)
{
  v <- cbind(tree$node_id, t(tree$params_sd))
  
  if (tree$caption == "TERMINAL")
  {
    return(v)
  }
  
  r <- se.rec(tree$right_child, leafs.only, level + 1)
  l <- se.rec(tree$left_child, leafs.only, level + 1)
  
  if (leafs.only) {
    data <- rbind(l, r)
    
  }
  if (!leafs.only) {
    data <- rbind(v, l, r)
    
  }
  
  return(data)
  
  
}
