setData <-
function(model, data)
{
#	print(paste("set data for ",model@name))
		if (is.null(model)) {
			warning("setData() was called with a NULL model");
			return(model);	
		}
	
	#	if (class(model) == "MxRAMModel") {

			model@data <- data;	

		#} else {
			
			if (length(model@submodels) > 0) {
				
				for (i in 1:length(model@submodels)) {
					#print("SET DATA RECURSIV *****")
					model@submodels[[i]] <- setData(model@submodels[[i]],data)	
				}
				
			}
									
		#}

	return(model);
}
