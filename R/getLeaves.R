getLeaves <- function(tree) {
  if (!isLeave(tree)) {
    left.list <- getLeaves(tree$left_child)
    right.list <- getLeaves(tree$right_child)
    return( c(left.list,right.list) )
  } else {
    return(list(tree))
  }
}