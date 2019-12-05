safeRunAndEvaluate <- function(model, return.model=F)
{
	# this is to trick the strict CRAN check which hates
  # OpenMx style, actually this can be removed in future
	objective <- NULL
	#
	
  if(inherits(model,"MxModel") || inherits(model,"MxRAMModel")){
    modelrun <- try(mxRun(model,silent=T, suppressWarnings=T),silent=T)
    if (is(modelrun,"try-error")) {
      return(NA)
    }  else {
      if (return.model) {
        result <- c()
        result$model <- modelrun
        result$LL <- getLikelihood(modelrun)
        return(result)
      } else {
        return(getLikelihood(modelrun))
      }
    }  
  }
  else if(inherits(model,"lavaan")){
    data <- data.frame(model@Data@X[[1]])
    colnames(data) <- model@Data@ov$name
    modelrun <- try(suppressWarnings(eval(parse(text=paste(model@Options$model.type,'(parTable(model),data=data,missing=\'',model@Options$missing,'\')',sep="")))),silent=T)
    #modelrun <- try(suppressWarnings(lavaan(parTable(model),data=data,model.type=model@Options$model.type,missing="fiml")),silent=T)
    if (is(modelrun,"try-error")) {
      #if(control$verbose){message("try error found...")}
      return(NA)
    }  
    else if(modelrun@Fit@converged) {
      if (return.model) {
        result <- c()
        result$model <- modelrun
        result$LL <- -2*lavaan::logLik(modelrun)
        return(result)
      } else {
        return(-2*lavaan::logLik(modelrun))
      }
    }
    else { return(NA)}
  }
}
