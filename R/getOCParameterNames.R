getOCParameterNames <- function(model, data, namefilter=NULL) {
	
		if (is.null(namefilter)) {
		  if (inherits(model,"MxModel") || inherits(model,"MxRAMModel")) {
			  namefilter <- model@manifestVars
		  }
      if (inherits(model,"lavaan")){
        namefilter <- model@Data@ov$name
      }
		}
	
		labels <- names(data)
		eqids <- which(labels %in% namefilter)
		values2<-labels[-which(labels %in% namefilter)]
		values1 <- labels[eqids]
		

		return( list(observed=values1, covariates=values2) )
	
}