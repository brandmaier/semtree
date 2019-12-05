setData <-
function(model, data)
{

		if (is.null(model)) {
			warning("setData() was called with a NULL model");
			return(model);	
		}
	
			model@data <- data;	

	    # if OpenMx model has submodels then call setData(...) to each submodel
			if (length(model@submodels) > 0) {
				
				for (i in 1:length(model@submodels)) {
					model@submodels[[i]] <- setData(model@submodels[[i]],data)	
				}
				
			}
									


	return(model);
}
