

gatherPredictors <- function(tree)
{
  if (is.null(tree)) { return(c()); }
  
  if (tree$caption=="TERMINAL") {
    return(c());
  } else {
    return(c(tree$rule$name,  gatherPredictors(tree$left_child),
             gatherPredictors(tree$right_child)))
  }
  
}

#' Count predictors
#'
#' This function is a generic to count the number of occurences of
#' predictors in either a 
#' \code{"semtree"} or a \code{"semforest"}. Note that this must not
#' be confused with the importance of those predictors. To estimate 
#' importance, rather use permutation-based variable importance with
#' \code{varimp}.
#'
#' @param x An object representing either a SEM tree or a forest.
#' @param ... Additional arguments passed to methods.
#'
#' @return A result depending on the method.
#'
#' @export
countPredictors <- function(x, ...) {
  UseMethod("countPredictors")
}

#' @rdname countPredictors
#' @exportS3Method countPredictors semtree
countPredictors.semtree <- function(x, ...)
{
  preds <- gatherPredictors(x)
  return(sort(table(preds), decreasing=TRUE))
}

#' @rdname countPredictors
#' @exportS3Method countPredictors semforest
countPredictors.semforest <- function(x, ...)
{
  return(sort(table(unlist(sapply(x$forest, gatherPredictors,simplify = TRUE))),decreasing=TRUE))
}
