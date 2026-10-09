getExpectedMean <- function(model) {
  if (is.null(attr(model$fitfunction$result, "expMean"))) {
    data <- data.frame(matrix(rnorm(100 * length(model$manifestVars)),
      nrow = 100, ncol = length(model$manifestVars)
    ))
    names(data) <- model$manifestVars
    omx <- mxModel(model, mxData(observed = data, type = "raw"))
    omx <- omxSetParameters(omx, labels = names(omxGetParameters(omx)), free = FALSE)
    model <- mxRun(omx, silent = TRUE)
  }

  dataMat <- attr(model$fitfunction$result, "expMean")
  return(dataMat)
}
