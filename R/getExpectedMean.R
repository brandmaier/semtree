getExpectedMean<- function(model) {
  
  
  # data <- simulateData(omx,10)
  # orsome fake data
  if (is.null(attr(model$fitfunction$result,"expMean"))) {
  
  data <- data.frame(matrix(rnorm(100*length(omx$manifestVars)),
                            nrow = 100,ncol=length(omx$manifestVars)))
  names(data) <- omx$manifestVars
  omx <- mxModel(omx, mxData(observed = data,type="raw"))
  omx <- omxSetParameters(omx, labels=names(omxGetParameters(omx)), free=FALSE)
  model <- mxRun(omx,silent = TRUE)
  
  }
  
  dataMat <- attr(model$fitfunction$result,"expMean")
  return(dataMat)
}