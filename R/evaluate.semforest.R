evaluate <- function (x, ...) {
  UseMethod("evaluate", x)
}

evaluate.semforest <- function(x, data=NULL, ...) 
{
  if (is.null(data)) {
    data <- x$data
  }
  
  eval.result <- simplify2array(
    lapply(X =forest$forest ,FUN=evaluateTree,test_set = data,data_type = "raw")
  )
  return(mean(unlist(eval.result[1,])))
}