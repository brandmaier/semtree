#' This function is a helper to determine which variables
#' in a data set are predictors and which are observed variables
#' in the template SEM
#' 
#' @param model A SEM
#' @param data A data.frame
#' 
#' @noRd
getOCParameterNames <- function(model, data, namefilter=NULL) {
	
		if (is.null(namefilter)) {
		  if (inherits(model,"MxModel") || inherits(model,"MxRAMModel")) {
			  namefilter <- model@manifestVars
		  }
      if (inherits(model,"lavaan")){
        namefilter <- model@Data@ov$name
      }
		}
	
    # get all column names from the data set
		labels <- names(data)
		# find column ids of columns with name matching namefilter 
		eqids <- which(labels %in% namefilter)
		values2<-labels[-which(labels %in% namefilter)]
		values1 <- labels[eqids]
		

		return( list(observed=values1, covariates=values2) )
	
}