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