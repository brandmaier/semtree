#'
#'
#' 
#mxAddNewModelData <- function(model=model, data=data, name="default", ...) {
#  return(mxModel(model,mxData(observed=data,type="raw"),name=name))
#}


#
mxAddNewModelData <- function(model=model, data=data, name="default", ...) {
  if(inherits(model,"MxModel") || inherits(model,"MxRAMModel")) {
    return(mxModel(model,mxData(observed=data,type="raw"),name=name))
  } else if(inherits(model,"lavaan")) {
      # must be a matrix it seems...      TODO: needs to be fixed I guess
      model@Data@X[[1]] <- data #as.matrix(data)
      model@Data@nobs[[1]] <- dim(data)[1]
      return(model)
  } else {
    stop("Not supported model type for AddNewModelData function.")
  }
}