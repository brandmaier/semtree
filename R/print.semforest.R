#' @exportS3Method print semforest
print.semforest <- function(x, ...) {
  invalid.trees <- sum(sapply(x$forest, FUN = is.null))


  cat(paste("SEM forest with ", length(x$forest), " trees.", "\n"))
  if (invalid.trees > 0) {
    cat(paste("Of these trees, ", invalid.trees, " trees are invalid due to errors.\n"))
  }
  
  cp <- countPredictors(x)
  cat("\nMost frequent predictors:\n")
  for (i in 1:min(length(cp),5)) {
    cat(names(cp[i]),":\t",cp[i],"\n")
  }

  hght <- sapply(x$forest, FUN=getHeight)  
  cat("\nTree Height:\n Mean:",mean(hght)," Median:", median(hght),"Min:",min(hght),"Max:",max(hght), "\n")
  perc_zero <- round(mean(hght==0)*100,2)
  cat(perc_zero,"% of the trees have no splits.\n")
}

#' @exportS3Method print semforest_stripped
print.semforest_stripped <- function(x, ...) {
  invalid.trees <- sum(sapply(x$forest, FUN = is.null))


  ui_message(paste("SEM forest [stripped] with ", length(x$forest), " trees.", "\n"))
  if (invalid.trees > 0) {
    ui_warn(paste("Of these trees, ", invalid.trees, " trees are invalid due to errors.\n"))
  }
}
