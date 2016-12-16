print.semforest <- function(x, ...)
{
 invalid.trees <- sum(sapply(x$forest,FUN=is.null))


 cat(paste("SEM forest with ",length(x$forest)," trees.","\n"))
 if (invalid.trees > 0) {
   cat(paste("Of these trees, ",invalid.trees," trees are invalid due to errors.\n"))
 }
 

}