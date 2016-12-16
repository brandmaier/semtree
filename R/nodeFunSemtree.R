
nodeFunSemtree<-function(x, labs, digits, varlen)
{
  paste(ifelse(x$frame$var=="<leaf>",x$frame$estimates, x$frame$yval), "\n\nN=",x$frame$n,x$frame$crit)
}

