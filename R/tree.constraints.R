
semtree.constraints <- function(local.invariance=NULL, global.invariance=NULL, focus.parameters=NULL)
{
  ret <- list()
  ret$local.invariance <- local.invariance
  ret$global.invariance <- global.invariance
  ret$focus.parameters <- focus.parameters
  class(ret) <- "semtree.constraints"
  
  return(ret)
  
}