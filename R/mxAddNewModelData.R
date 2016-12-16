mxAddNewModelData <- function(model=model, data=data, name="default", ...) {
  return(mxModel(model,mxData(observed=data,type="raw"),name=name))
}