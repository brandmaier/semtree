#
# obtain expected covariance matrix from a sempower model
#

getExpectedCovariance <- function(model) {


 # if (inherits(model,"semper")) {  
#   omx <- getOpenMxRepresentation(model)
#  } else if (inherits(model,"MxRAMModel")) {
#    omx <- model
#  } else {
#   stop("Unknown model type!")
#  }

 # data <- simulateData(omx,10)
  # orsome fake data
  if (is.null(attr(model$fitfunction$result,"expCov"))) {
  data <- data.frame(matrix(rnorm(100*length(omx$manifestVars)),
                            nrow = 100,ncol=length(omx$manifestVars)))
  names(data) <- omx$manifestVars
  
  
  
  omx <- mxModel(omx, mxData(observed = data,type="raw"))
  omx <- omxSetParameters(omx, labels=names(omxGetParameters(omx)), free=FALSE)
  model <- mxRun(omx,silent = TRUE)
  }
    #sm <- summary(run)
  dataMat <- attr(model$fitfunction$result,"expCov")
  return(dataMat)
}
