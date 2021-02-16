#' @exportS3Method print summary.semtree
print.summary.semtree <-
function(x, ...) {
  cat("SEMtree Summary\n");	
  cat("Template model: ",x$model.title,"\n")
  cat("Total Sample Size: ", x$sample.size,"\n")
  cat("Number of nodes: ", x$num.nodes,"\n")
  cat("Number of leaf nodes: ", x$leaf.nodes,"\n")
  cat("Free Parameters: ", x$df," (",x$parameter.names,")\n")
}
