outliers <- function(prox)
{
#	plot(hclust(as.dist(prx)))
  ol <- dim(prox)[1]/rowSums(prox^2)
  
  #ol <- (ol - mean(ol)) / sqrt(var(ol))
	
  med <- median(ol)
  dev <- median(abs(ol-med))
  
  ol <- (ol-med) / dev
  
  ol[ol<0] <- 0
  
  return(ol)
}