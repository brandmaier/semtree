getLeafs <- function(tree, data=NULL) {
  if (!is.null(data)) {
   return(traverse(tree, data))
  }

  if (!isLeaf(tree)) {
    left.list <- getLeafs(tree$left_child)
    right.list <- getLeafs(tree$right_child)
    return( c(left.list,right.list) )
  } else {
    return(list(tree))
  }
}