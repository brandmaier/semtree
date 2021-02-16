#' Quantify bio diversity of a SEM Forest
#' 
#' A function to calculate biodiversity of a \code{\link{semforest}} object.
#' 
#' 
#' @param x A \code{\link{semforest}} object
#' @param aggregate.fun Takes a function to apply to the vector of pairwise
#' diversities. By default, this is the median.
#' @author Andreas M. Brandmaier
#' @keywords biodiversity semforest
#' @export
biodiversity <- function(x, aggregate.fun=median)
{
  if (!(
        is(x,"semforest") | 
        is(x,"diversityMatrix")
      ))
  {
    stop("Error! x must be a SEM forest or a diversity matrix!")
  }  
  
  if (is(x,"semforest"))  {
    message("Computing diversity matrix.")
    D <- diversityMatrix(x)
  } else {
    D <- x
  }
  
  values <- D[lower.tri(D)]
  
  return(aggregate.fun(values))
  
}
