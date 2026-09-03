#' Aggregate Variable Importance Estimates
#' 
#' This function aggregates variable importance estimates over
#' trees. It is a helper function used when print() is called
#' on a variable importance estimate from a SEM forest.
#' 
#' @param vimp Variable importance estimate from a SEM forest.
#' @param aggregate Character. Either 'mean' or 'median' as function to aggregate estimates over a forest
#' @param scale Character. Either 'absolute' or 'relative'.
#' @param omit.na Boolean. By default TRUE, which ignores NA estimates when aggregating. Otherwise they are interpreted as zero.
#' @param scale.by Integer. By default 1. Scaling parameter.
#' @export
#' 
aggregateVarimp <-
  function(vimp,
           aggregate = c("mean","median"),
           scale = c("absolute","relative.baseline"),
           omit.na = TRUE, scale.by=1)
  {
    aggregate <- match.arg(aggregate)
    scale <- match.arg(scale)
    
    if (is(vimp, "semforest.varimp")) {
      datamat <- vimp$importance
    } else {
      datamat <- vimp
    }
    
    # omit NA
    if (!omit.na) {
      datamat[is.na(datamat)] <- 0
    }
  
    if (scale.by!=1) {
      datamat <- datamat * scale.by
      vimp$ll.baseline <- vimp$ll.baseline * scale.by
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
      

      
      data <-
#        -100 + (datamat + baseline.matrix) * 100 / baseline.matrix
      datamat / baseline.matrix * 100
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
