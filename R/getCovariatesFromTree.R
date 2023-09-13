# return the names of all covariates that are used for splitting nodes in a given tree
getCovariatesFromTree <- function(tree) {
  if (tree$caption == "TERMINAL") {
    return()
  } else {
    l <- getCovariatesFromTree(tree$left_child)
    r <- getCovariatesFromTree(tree$right_child)
    return(unique(c(l, r, tree$rule$name)))
  }
}