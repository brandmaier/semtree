stripTree <- function(tree)
{
  # delete data from OpenMx model
    #tree$model@data <- NULL
    tree$model@data$observed <- tree$model@data$observed[0,]
    tree$result <- NULL
    #tree$model$A <- NULL
    #tree$model$S <- NULL
    #tree$model$F <- NULL
    #tree$model$M <- NULL
    
    if (tree$caption!="TERMINAL") {
      tree$left_child <- stripTree(tree$left_child)
      tree$right_child <- stripTree(tree$right_child)
    }
    
    return(tree);
}