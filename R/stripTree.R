stripTree <- function(tree)
{
  # delete data from OpenMx model
    tree$model@data <- NULL

    tree$left_child <- stripTree(tree$left_child)
    tree$right_child <- stripTree(tree$right_child)

    return(tree);
}