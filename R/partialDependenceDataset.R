partialDependenceDataset <- function(covarFrame, refCovar, numGrid, plotRange = NULL){
  #
  covars <- colnames(covarFrame)
  refIndex <- which(covars == refCovar)
  refVar <- covarFrame[,refIndex]
  if (is.null(plotRange)){
    start <- min(refVar)
    end <- max(refVar)
  } else {
    start <- plotRange[1]
    end <- plotRange[2]
  }
  grid <- seq(start, end, length=numGrid)
  #
  outFrame <- covarFrame
  outFrame[,refIndex] <- grid[1]
  for (i in 2:numGrid){
    upFrame <- covarFrame
    upFrame[,refIndex] <- grid[i]
    outFrame <- rbind.data.frame(outFrame,upFrame)
  }
  return(outFrame)
}
