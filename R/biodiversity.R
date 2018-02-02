biodiversity <- function(x, aggregate.fun=median)
{
  if (!(
        (any(class(x)=="semforest")) | 
        (any(class(x)=="diversityMatrix"))
      ))
  {
    stop("Error! x must be a SEM forest or a diversity matrix!")
  }  
  
  if (any(class(x)=="semforest"))  {
    message("Computing diversity matrix.")
    D <- diversityMatrix(x)
  } else {
    D <- x
  }
  
  values <- D[lower.tri(D)]
  
  return(aggregate.fun(values))
  
}