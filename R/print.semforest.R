#' @exportS3Method print semforest
print.semforest <- function(x, ...)
{
 invalid.trees <- sum(sapply(x$forest,FUN=is.null))


 cat(paste("SEM forest with ",length(x$forest)," trees.","\n"))
 if (invalid.trees > 0) {
   cat(paste("Of these trees, ",invalid.trees," trees are invalid due to errors.\n"))
 }
 

}

#' @exportS3Method print semforest_stripped
print.semforest_stripped <- function(x, ...)
{
  invalid.trees <- sum(sapply(x$forest,FUN=is.null))
  
  
  cat(paste("SEM forest [stripped] with ",length(x$forest)," trees.","\n"))
  if (invalid.trees > 0) {
    cat(paste("Of these trees, ",invalid.trees," trees are invalid due to errors.\n"))
  }
  
  
}