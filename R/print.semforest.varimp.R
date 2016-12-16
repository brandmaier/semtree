
print.semforest.varimp <- function(x, aggregate="mean",
                                   scale="absolute", sort.values=F,
                                   na.omit=FALSE, ...) {
  
  vimp <- x
  
  if (class(vimp$importance)=="matrix") {
    x <- aggregateVarimp(vimp, aggregate, scale, na.omit)
  } else {
    x <- vimp$importance
    
    if (!na.omit) {
      x[is.na(x)] <- 0
    }
    
  }
  
  
  if (sort.values) {
    
    # replace NAs with low number
    low <- min(x,na.rm=T)-1
    filt <- is.na(x)
    x[filt] <- low
    
    srt <- sort(x, index.return=T)
    x <- x[srt$ix]
    
    # vnames <- vnames[srt$ix]
    
    x[x<=(low+.5)] <- NA
  }
  
  
  
  #x <-data.frame(x)
  #names(x) <-  vimp$var.names
  cat("Variable Importance\n")
  print(x, ...)
}