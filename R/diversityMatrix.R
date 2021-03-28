#' Diversity Matrix
#' 
#' Computes a diversity matrix using a distance function between trees
#' 
#' 
#' @param forest A SEM forest
#' @param divergence A divergence function such as hellinger or klsym
#' @param showProgressBar Boolean. Show a progress bar.
#' @export
diversityMatrix <- function(forest, divergence=klsym, showProgressBar=TRUE) {
  
  
  
  trees <- forest$forest
  
  ntree <- length(trees)
  distmat <- matrix(NA,nrow=ntree,ncol=ntree)
  
  if (showProgressBar)
  pb <- utils::txtProgressBar(min=0,max= (ntree^2-ntree)/2)
  
  cnt <- 1
  for (i in 1:ntree) {
    for (j in i:ntree) {
      
      if (i==j) {distmat[i,j]<-0; next;}
      
      tsi <- NA
      tryCatch({
        tsi <- treeDivergence(trees[[i]],trees[[j]], forest$data,
                            divergence=divergence)
      })
      
      distmat[i,j] <- distmat[j,i] <- tsi
      
      if (showProgressBar)
        setTxtProgressBar(pb, cnt)
      cnt <- cnt + 1
    }
  }
  
  if (showProgressBar)
    close(pb)
  
  class(distmat) <- "diversityMatrix"
  
  return(distmat)
}
