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
	
	# defaults
	# TODO: implement semtrees focus parameter interface (AB)
	parameter <- NULL

#   if (!is.factor(covariate)) {
#     level <- "metric"
#     method <- control$score.tests["metric"]  # default: CvM
#   } else {
#     if (is.ordered(covariate)) {
#       level <- "ordinal"
#       method <- control$score.tests["ordinal"] # default: "maxLM" 
#     } else {
#       level <- "nominal"
#       method <- control$score.tests["nominal"] # default: "LM" 
#     }
#   }
# 	
# 	if (!all(c(complete.cases(fit$data$observed)), complete.cases(covariate))) {
# 	stop("Incomplete data")
# 	}
	
	print("Call")
	
	# main call to score test
	test.result <- scoretest(fit=fit,
	                         covariate=covariate,
	                         score_tests=control$score.tests,
	                         alpha=control$alpha)
	                         #min.bucket = control$min.bucket)
	
	#######TODO Für einhetlichen Output
	
	  ts <- test.result$`Test statistic`
	  splt <- test.result$`Cut point`
	  pval <- test.result$`p-value`
	
	cur.name <- colnames(mydata)[cur_col]
	
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