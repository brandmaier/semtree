
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
