varimpTree <- function(tree,
                       data,
                       var.names = NULL,
                       verbose = FALSE,
                       max.level = NA,
                       eval.fun = evaluateTree,
                       method="permutation", 
                       conditional=FALSE) {
  # prune tree back to given level if "max.level" is specified
  if (!is.na(max.level)) {
    tree <- prune(tree, max.level)
  }
  
  
  # prepare storage for results
  total <- rep(0, length(var.names))
  
  oob.data <- data$oob.data
  
  # obtain likelihood of unpermuted data
  ll.baseline <- eval.fun(tree, oob.data)$deviance
  if (verbose) {
    cat("LL baseline", ll.baseline, "\n")
  }
  
  
  treecovs <- getCovariatesFromTree(tree)
  
  # all covariates
  for (cov.name in var.names) {
    index <- which(var.names == cov.name)
    
    if (!cov.name %in% treecovs) {
      total[index] <- NA
      
      next
      
    } else {
      
      # permute variable with name "cov.name" in data and store result in "oob.data.permuted"
      permutation.idx <- which(cov.name == names(data$oob.data))
      oob.data.permuted <- oob.data
      
     # browser()
      
      # random permutation
      if (!conditional) {
      col.data <- oob.data.permuted[, permutation.idx]
      oob.data.permuted[, permutation.idx] <-
        sample(col.data, length(col.data), replace = F)
      } else {
        stop("Not yet implemented!")
        
        oob.data.permuted <- conditionalSample(tree, oob.data.permuted, permutation.idx)
        
        if (all(oob.data.permuted == oob.data)) {
          print("Warning! OOB DATA RESAMPLING HAD NO EFFECT")
        }
        
        #browser()
       # stop("Not implemented yet!")
      }
     
      # obtain likelihood of permuted data
      if (method=="permutation") {
        ll.permuted <- eval.fun(tree, oob.data.permuted)$deviance
      } else if (method=="permutationInteraction") {
        ll.permuted <- evaluateTreeConditional(tree, list(oob.data.permuted, oob.data))$deviance        
      } else {
        stop("Error. Method is not fully implemented!")
      }

      
      if (verbose) {
        cat(cov.name, "LL permuted", ll.permuted, "\n")
      }
      
      total[index] <- -ll.baseline + ll.permuted
    }
    
  }
  
  return(list(total = total, ll.baseline = ll.baseline))
  
  
  #		}, error=function(e) {
  #			cat(paste("Error in tree #","\n",e));
  #			return(list(total=rep(NA, length(var.names)),ll.baseline=NA));
  #	});
  
}

#varimp.tree.test <- function(tree, data, vn, vb) {
#	cat(tree$N)
#}

varimp <- function(forest,
                   var.names = NULL,
                   verbose = F,
#                   main.effects = F,
                   cluster = NULL,
                   eval.fun = evaluateTree,
                   method="permutation",
                   conditional=FALSE,
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
  if (method=="prune") {
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
      t(t(result$ll.baselines)) # this is stupid, should be as.matrix?!
  }
  colnames(result$importance) <- var.names
  result$var.names <- var.names
  class(result) <- "semforest.varimp"
  return(result)
  
  
}

aggregateVarimp <-
  function(vimp,
           aggregate = "mean",
           scale = "absolute",
           omit.na = TRUE)
  {
    if (is(vimp, "semforest.varimp")) {
      datamat <- vimp$importance
    } else {
      datamat <- vimp
    }
    # omit NA
    
    if (!omit.na) {
      datamat[is.na(datamat)] <- 0
    }
    
    # rescale ?
    if (scale == "absolute") {
      data <- datamat
    } else if (scale == "relative.baseline") {
      baseline.matrix <-
        matrix(
          rep(vimp$ll.baseline, each = dim(datamat)[2]),
          ncol = dim(datamat)[2],
          byrow = T
        )
      #data <- 100-baseline.matrix*100/(vim$importance + baseline.matrix)
      data <- -100 + (datamat + baseline.matrix) * 100 / baseline.matrix
    } else {
      stop("Unknown scale. Use 'absolute' or 'relative.baseline'.")
      
    }
    
    if (aggregate == "mean") {
      x <- colMeans(data, na.rm = TRUE)
    } else if (aggregate == "median") {
      x <- colMedians(data, na.rm = TRUE)
    } else {
      stop("Unknown aggregation function. Use mean or median")
      
    }
    
    return(x)
    
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
