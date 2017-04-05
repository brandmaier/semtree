evaluateDataLikelihood <-
function(model, data, data_type="raw")
{
  if(inherits(model,"MxModel") || inherits(model,"MxRAMModel")) {
  
	# this is to trick the strict CRAN check
	objective <- NULL
	#
	
		#
		# Fix all Matrices A,S,F and M
		#
		#print("****")
		#print(class(model))
		#model <- fix_model(model);
	
		model <- omxSetParameters(model, labels=names(omxGetParameters(model)),free=F)
	
		#
		# run model (without any free params) on dataset
		#
		if (data_type == "raw")
		{
		
		data <- full_mxdata <- mxData(
        observed=data,
        type="raw"
   		 );
   		 				
		} else if (data_type =="cov") {
			data <- full_mxdata <- mxData(
       		observed=cov(data),
        	type="cov",
        	numObs=dim(data)[1]
    		);    				
		} else if (data_type == "cor") {
			data <- full_mxdata <- mxData(
       		observed=cor(data),
        	type="cor",
        	numObs=dim(data)[1]
    		);			
			
		} else {
			warning("data type is not supported!");			return(NULL);
			
		}
		
		
		
		model <- setData(model, data)
		
		run <- OpenMx::mxRun(model, silent=T, useOptimizer=F, suppressWarnings=T);
		result <- getLikelihood(run)

		
		
		
	
		
	
		return(result);
		
		
  } else {
    stop("The chosen combination of parameters for semtree is not yet supported with lavaan! Please use OpenMx model specification!")
  }
}
