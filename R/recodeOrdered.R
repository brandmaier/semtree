recodeOrdered <-
function(v, name="unknown", id=NA) {
	
	result <- c()
	
	values_set <- sort(union(v,v))
	
	num_sets <- length(values_set)
	
	if (num_sets == 1) {
		return(NULL); # NULL;	
	}
	
	if (all(is.na(v))) {
		return(NULL)	
	}
	
	columns <- c()
	names <- c()
	expressions <- c()
	
	for (i in 2:num_sets)
	{
		#print(values_set[i])
		#print(values_set[i-1])
		
		if (is.double(values_set[i])) {
			threshold <- (values_set[i-1]+values_set[i])/2.0
		} else {
			threshold <- values_set[i]
		}
		
		column <- ifelse(v<threshold,1,0)
		
		#columns <- append(columns, column)
		columns <- cbind(columns, column)
		names <- append(names, paste(name,"<",toString(threshold)))
		
		exp <- list()
		exp$value <- threshold
		exp$type <- "<"
		exp$id <- id
		
		expressions <- append(expressions, exp);
		
	}	

	#result$columns <- columns;
	#result$names <- names;
	columns <- data.frame(columns);
	names(columns) <- names;
	
	result <- c();
	result$columns <- columns;
	result$expressions <- expressions;
	
	return(result);
	
}
