nodeFunSemtree <- function(x, labs, digits, varlen) {
  paste(
    ifelse(x$frame$var == "<leaf>", x$frame$estimates, x$frame$yval), "\n\n",
    paste0("N=", x$frame$n), x$frame$crit
  )
}
