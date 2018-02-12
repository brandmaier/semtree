getOOBWeights <- function(forest) {
  misfit <- mapply(forest$forest,forest$forest.data, FUN= function(tree, data) {
    evaluateTree(tree=tree, test_set=data$oob.data,data_type = "raw")
  })
  weights <- 1/unlist(misfit[1,])
  weights <- weights / sum(weights)
  
  return(weights)
}