#' Aggregate Variable Importance Estimates
#' 
#' This function aggregates variable importance estimates over
#' trees. It is a helper function used when print() is called
#' on a variable importance estimate from a SEM forest.
#' 
#' @param aggregate Character. Either 'mean' or 'median' as function to aggregate estimates over a forest
#' @param scale Character. Either 'absolute' or 'relative'.
#' @param omit.na Boolean. By default TRUE, which ignores NA estimates when aggregating. Otherwise they are interpreted as zero.
#' 
#' @export
#' 
aggregateVarimp <-
  function(vimp,
           aggregate = c("mean","median"),
           scale = c("absolute","relative.baseline"),
           omit.na = TRUE)
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
      data <-
        -100 + (datamat + baseline.matrix) * 100 / baseline.matrix
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
