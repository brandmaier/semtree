#' SEMtree Partitioning Tool
#' 
#' The \code{subtree} function returns a tree from a selected node of the
#' \code{\link{semtree}} returned tree.
#' 
#' The row names of the resulting data frame correspond to internal node ids
#' and the column names correspond to standard errors in the SEM. Standard
#' errors of the estimates can be obtained from \code{\link{se}}.
#' 
#' @param tree A SEMtree object obtained from \code{\link{semtree}}
#' @param startNode Node id, which will be future root node (0 to max node
#' number of \code{tree)}
#' @param level Ignore. Only used internally.
#' @param foundNode Ignore. Only used internally.
#' @return Returns a \code{\link{semtree}} object which is a partitioned tree
#' from the input \code{semtree}.
#' @author Andreas M. Brandmaier, John J. Prindle
#' @seealso \code{\link{semtree}}, \code{\link{semtree.control}}
#' @references Brandmaier, A.M., Oertzen, T. v., McArdle, J.J., & Lindenberger,
#' U. (2013). Structural equation model trees. \emph{Psychological Methods},
#' 18(1), 71-86.
#' @export
subtree <-
  function(tree, startNode=NULL, level=0, foundNode=FALSE)
  {
    if(is.null(startNode)){
      warning("No starting node (startNode) for subtree provided.")
      return(NULL)
    }
    if (startNode==level&level==0){
      warning("Starting .node is the same as a full tree")
      return(tree)
    }
    
    if(tree$caption!="TERMINAL"){
      if(tree$node_id==startNode){
        foundNode <- TRUE
        tree$traverse.fun <- NULL
        tree$traverseRow.fun <- NULL
        return(tree)
      }
      else{
        if(!foundNode)
        l <- subtree(tree$left_child, startNode, level+1, foundNode)
        r <- subtree(tree$right_child, startNode, level+1, foundNode)
      }
      if(!is.null(l)){
        return(l)
      }
      else if(!is.null(r)){
        return(r)
      }
    }
  }

# tree <- subtree(result2, startNode=9)
# plot(tree)
