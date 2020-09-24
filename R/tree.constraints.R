
semtree.constraints <- function(local.invariance=NULL, global.invariance=NULL, focus.parameters=NULL)
{
  ret <- list()
  ret$local.invariance <- local.invariance
  ret$global.invariance <- global.invariance
  ret$focus.parameters <- focus.parameters
  class(ret) <- "semtree.constraints"
  
  return(ret)
  
}

print.semtree.constraints <- function(x, ...) {
  cat("semtree constraints\n")
  cat(" Local invariance ",paste0(x$local.invariance),"\n")
  cat(" Global invariance ",paste0(x$global.invariance),"\n")
  cat(" Focus parameters ",paste0(x$focus.parameter),"\n")
}