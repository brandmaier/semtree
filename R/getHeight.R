getHeight <- function(tree)
{
  if ((is.null(tree$left_child)) && (is.null(tree$right_child))) 	
  {
    return(1);
  }
  
  if (tree$left_child$caption != "TERMINAL") {
    countl <- 1+ getHeight(tree$left_child)
  } else {
    countl <- 2	
  }
  if (tree$right_child$caption != "TERMINAL") {
    countr <- 1+ getHeight(tree$right_child)
  } else {
    countr <- 2	
  }	
  
  return( max(countl, countr) )
}

getDepth <- getHeight
