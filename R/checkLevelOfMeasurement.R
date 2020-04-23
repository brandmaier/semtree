checkLevelOfMeasurement <- function(x) {
  
  if (is.data.frame(x) || is.matrix(x)) {
    
    for (i in 1:ncol(x)) {
      
      if (!is.factor(x[,i])) {
        num.unique.vals <- length(unique(x[,i]))
        if (num.unique.vals < 50 && num.unique.vals < sqrt(length(x[,i]))) {
          ui_warn("Level of measurement. Consider specifying the ")      
        }
      }
      
    }
    
    return(TRUE)
    
  } else {
    ui_warn("Could not check scales.")
    return(TRUE)
  }
  
}