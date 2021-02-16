#' Find outliers based on case proximity
#' 
#' Compute outlier score based on proximity matrix.
#' 
#' 
#' @param prox A proximity matrix.
#' @author Andreas M. Brandmaier, John J. Prindle
#' @seealso \code{\link{proximity}}
#' @references Brandmaier, A.M., Oertzen, T. v., McArdle, J.J., & Lindenberger,
#' U. (2013). Structural equation model trees. \emph{Psychological Methods},
#' 18(1), 71-86.
#' @export
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
