#
# 
#
partialDependenceDataset <- function(covarFrame, refCovar, xgrid, plotRange = NULL){
  #
  covars <- colnames(covarFrame)
  refIndex <- which(covars == refCovar)
  refVar <- covarFrame[,refIndex]
  
  #
  outFrame <- covarFrame
  outFrame[,refIndex] <- xgrid[1]
  numGrid <- length(xgrid)
  for (i in 2:numGrid){
    upFrame <- covarFrame
    upFrame[,refIndex] <- xgrid[i]
    outFrame <- rbind.data.frame(outFrame,upFrame)
  }
  return(outFrame)
}
