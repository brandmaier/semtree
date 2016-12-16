findOtherSplits <-
function(node, tree)
{
	if (tree$p.values.valid) {
		idx <- which(node$p_values <= tree$options$alpha)
	} else {
		idx <- which(node$lr_values > 0)
	}
	
	idx2 <- node$covariate.ids[idx]
	
	cvalues <- as.numeric( round( node$lr_values[idx], 3) )
	cnames <- names(tree$recoding$dataset)[idx2]
	
	#cat(cvalues)
	#cat(cnames)
	#print(cvalues)
	
	df<-data.frame( cbind(cnames,  cvalues ) )
	df<-data.frame( cnames, cvalues  )
	
	#df <- sort(df, by =~ df[,2])
	df <- df[with(df,order(-df[,2])) ,]
	
	return(df)
}
