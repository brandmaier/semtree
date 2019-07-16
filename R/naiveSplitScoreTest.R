naiveSplitScoreTest <- function(model=NULL, mydata=NULL, control=NULL, 
                               invariance=NULL, meta=NULL, 
                               pp=FALSE, constraints=NULL, ...) {


if(control$sem.prog != 'OpenMx'){ stop("Score Test is not implemented for lavaan models yet.") }
	
# TODO
# - test for invariance
# - test for constraints
    
 if(pp) {cmp.column.ids <- max(meta$model.ids+1)}
    else {cmp.column.ids <- meta$covariate.ids}
		
LL.max <- -Inf
split.max <- NA
col.max <- NA
name.max <- NA
type.max <- NA
p.max <- NA
contrib.max <- NA
    
# fit model once to complete data
fit <- mxAddNewModelData(model,mydata,name="BASE MODEL")
fit <- mxRun(fit,silent = TRUE)
#LL.overall <- safeRunAndEvaluate(fit) 
#suppressWarnings(if (is.na(LL.overall)) return(NULL))
	
# main loop with calls to scoreTest(...)
for (cur_col in cmp.column.ids) {					   
	
	covariate <- mydata[,cur_col]
	cur.name <- colnames(mydata)[cur_col]
	
	# defaults
	# TODO: implement semtrees focus parameter interface (AB)
	parameter <- NULL

	print("Call")
	
	# main call to score test
	test.result <- scoretest(fit=fit,
	                         covariate=covariate,
	                         score_tests=control$score.tests,
	                         alpha=control$alpha,
	                         min_obs_left=control$min.bucket,
	                         min_obs_right=control$min.bucket)
	
	# TODO AB: What to do if a structural break occured after the first or
	# the last values of the sorted covariate.
	# Warning message
	if (test.result$`Groups too small` == "left" & test.result$`H0 rejected`) {
	  warning(paste("Structural break occured within the first", min.bucket,
	              "observations! Covariate used:", cur.name), call. = FALSE)
	}
	if (test.result$`Groups too small` == "right" & test.result$`H0 rejected`) {
	  warning(paste("Structural break occured within the last", min.bucket,
	              "observations! Covariate used:", cur.name), call. = FALSE)
	}
	
	# TODO AB: If the covariate is nominal and has three levels or more, use
	# likelihood ratio semtree implementation to determine cut point.


	
	#######TODO Für einhetlichen Output
	
	  ts <- test.result$`Test statistic`
	  splt <- test.result$`Cut point`
	  pval <- test.result$`p-value`
	
	
	
	# determine type
	cur.type <- 2
	if (is.factor(covariate)) {
	  if (!is.ordered(covariate)) {
	    cur.type <- 1
	  } else {
	    cur.type <- 2
	  }
	}
	
	if (control$verbose) {
	cat("Testing: ", cur.name,"\n")
	cat("Test statistic: ",ts, "(CvM",test.result$`CvM.Test statistic`,")",
	    " best so far: ",name.max," with ",LL.max, " at ", splt," as split point",
	    "\n")
	cat("p value: ",pval,"\n")
	}
	
	if (ts > LL.max) {
		LL.max <- ts
		split.max <- splt
		col.max <- cur_col
		name.max <- cur.name
		type.max <- cur.type
		p.max <- pval
		contrib.max <- test.result$DM.Contributions
	}
}

n.comp <- length(cmp.column.ids)

# format results
return(list(LL.max=LL.max,split.max=split.max,name.max=name.max,
            col.max=col.max, type.max=type.max, n.comp=n.comp, btn.matrix=NULL, 
            invariance.filter=NULL, p.max = p.max, contrib.max=contrib.max))
					   
}