convergenceStatus <- function(tree)
{
  if (is.null(tree)) { return(c()); }
  
  status <- hasConverged(x$model)

  if (tree$caption=="TERMINAL") {
    return(status);
  } else {
    return(c(status,  convergenceStatus(tree$left_child),
             convergenceStatus(tree$right_child)))
  }
}
