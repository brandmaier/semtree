#' Average Deviance of a Dataset given a Forest
#' 
#' Evaluates the average deviance (-2LL) of a dataset given a forest.
#' 
#' 
#' @aliases evaluate.semforest evaluate
#' @param x A fitted \code{\link{semforest}} object
#' @param data A data.frame
#' @param \dots No extra parameters yet.
#' @return Average deviance
#' @author Andreas M. Brandmaier
#' @seealso \code{\link{evaluateDataLikelihood}}, \code{\link{semtree}},
#' \code{\link{semforest}}
#' @references Brandmaier, A.M., Oertzen, T. v., McArdle, J.J., & Lindenberger,
#' U. (2013). Structural equation model trees. \emph{Psychological Methods},
#' 18(1), 71-86.
#' @exportS3Method evaluate semforest
evaluate <- function (x, data=NULL, ...) {
  UseMethod("evaluate", x)
}

evaluate.semforest <- function(x, data=NULL, ...) 
{
  if (is.null(data)) {
    data <- x$data
  }
  
  eval.result <- simplify2array(
    lapply(X =x$forest ,FUN=evaluateTree,test_set = data,data_type = "raw")
  )
  
  if (is.null(x$weights)) {
    return(mean(unlist(eval.result[1,])))
  } else {
    return( sum(x$weights * unlist(eval.result[1,])) )
  }
  

}
