missingDataModel <- function(model, mydata, c)
{
  ############################################################
  # include for lavaan as well
  ############################################################
  paredData<-mydata[!is.na(mydata[,c]),]
  if(nrow(paredData)==nrow(mydata)){ return(NULL)}
  else{
    if(inherits(model,"MxModel") || inherits(model,"MxRAMModel")) {
  
          newModel <- mxModel(model,mxData(paredData,type="raw"))
        
    }
    else if(inherits(model,"lavaan")) {
      
      #if (!is.null(constraints)) {
      #  if (!is.null(constraints$focus.parameters)) {
      #    stop("Sorry! Lavaan is currently not supported with option constraints$focus.parameters!")
      #  }
      #}
      
      newModel <- lavaan::lavaan(lavaan::parTable(model),data=paredData,model.type=model@Options$model.type,do.fit=FALSE)
    }
    return(newModel)
  }
}