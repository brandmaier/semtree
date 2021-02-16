#' @exportS3Method 
summary.semforest <- function(object, ...)
{
  
  height.distr <- table(
    simplify2array(lapply(object$forest, getHeight)))
  cat(paste("Height distribution of the forest:"))
  print(height.distr)
  
}