varimp <- function(forest,
                   var.names = NULL,
                   verbose = F,
                   #                   main.effects = F,
                   cluster = NULL,
                   eval.fun = evaluateTree,
                   method = "permutation",
                   conditional = FALSE,
                   ...)
{
  if ("parallel" %in% list(...)) {
    warning(
      "Use of snowfall is deprecated and must be replaced with cluster argument from package 'parallel'! See manual"
    )
  }
  
  if (is.null(var.names)) {
    var.names <- forest$covariates
  }
  
  
  if (method == "permutation" &&
      !is.null(forest$constraints$focus.parameters))
  {
    ui_warn(
      "Consider switching to method='permutationFocus' because forest has focus parameters."
    )
    
    #ui_warn("Switching to method='permutationFocus' because forest has focus parameters.")
    #method = "permutationFocus"
  }
  
  
  
  result <- list()
  start.time <- proc.time()
  
  if (is.null(cluster)) {
    temp <- mapply(
      FUN = varimpTree,
      forest$forest,
      forest$forest.data,
      MoreArgs = list(
        var.names = var.names,
        verbose = verbose,
        max.level = NA,
        eval.fun = eval.fun,
        method = method,
        conditional = conditional
      ),
      SIMPLIFY = FALSE,
      USE.NAMES = TRUE
    )
  } else {
    temp <-
      parallel::clusterMap(
        cl = cluster,
        fun = varimpTree,
        forest$forest,
        forest$forest.data,
        MoreArgs = list(
          var.names = var.names,
          verbose = verbose,
          max.level = NA,
          eval.fun = eval.fun,
          method = method
        ),
        SIMPLIFY = FALSE,
        USE.NAMES = TRUE
      )
  }
  
  
  
  elapsed <- proc.time() - start.time
  
  # extract results and put them into result-object
  result$ll.baselines <-
    sapply(temp, function(x) {
      try({
        x$ll.baseline
      })
    })
  result$importance <-
    t(sapply(temp, function(x) {
      try({
        x$total
      })
    }))
  result$elapsed <- elapsed
  
  # completeley experimental, probably not a wise idea to use this
  if (method == "prune") {
    temp <- mapply(
      varimpTree,
      forest$forest,
      forest$forest.data,
      MoreArgs = list(
        var.names = var.names,
        verbose = verbose,
        max.level = 1
      ),
      SIMPLIFY = FALSE,
      USE.NAMES = T
    )
    
    result$importance.level1 <-
      t(sapply(temp, function(x) {
        x$total
      }))
    colnames(result$importance.level1) <- var.names
  }
  
  if (dim(result$importance)[1] == 1) {
    #result$importance<-t(result$importance)
    result$ll.baselines <-
      t(t(result$ll.baselines)) # TODO: this is stupid, should be as.matrix?!
  }
  
  colnames(result$importance) <- var.names
  result$var.names <- var.names
  class(result) <- "semforest.varimp"
  return(result)
  
  
}


colMedians <- function(x, na.rm = TRUE)
{
  return(apply(
    X = x,
    FUN = function(x) {
      median(x, na.rm = na.rm)
    },
    MARGIN = 2
  ))
}
