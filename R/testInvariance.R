# not implemented in code////

testInvariance <-
function(original_model, sub_model1, sub_model2, sub1_mxdata, sub2_mxdata, verbose=FALSE, alpha.level=0.05, invariance=NULL)
{
		if (verbose) {
    	  print(paste("Performing additional invariance test with " ));
		}
		
	#	if (is.null(invariance)) {
	#		print("Cannot perform invariance test without specified variables")
	#		return(NULL);	
	#	}

		# fit original model with subsets (and no restrictions)
    	original_model$data <- sub1_mxdata;
    	try (
    	 run_sub1_original <- OpenMx::mxRun(original_model, suppressWarnings=T, silent=T) 
    	 )
    	original_model$data <- sub2_mxdata;
    	try( run_sub2_original <- OpenMx::mxRun(original_model, suppressWarnings=T, silent=T) )    	
    	# get summaries of the sub model outputs (w/o constraints)
   	 	sum_sub1_original <- summary(run_sub1_original);
    	sum_sub2_original <- summary(run_sub2_original);
    	
    	# get summaries of the sub model outputs (with constraints)
    	sum_sub1 <- summary(sub_model1)
    	sum_sub2 <- summary(sub_model2)
    	
    	# calculate degrees of freedoms for original models and the fitted models
    	df1 <- sum_sub1_original$estimatedParameters + sum_sub2_original$estimatedParameters
    	df2 <- sum_sub1$estimatedParameters + sum_sub2$estimatedParameters

    	df_invariance <- df1-df2
    	lr_invariance <- +sum_sub1$Minus2LogLikelihood + sum_sub2$Minus2LogLikelihood -sum_sub1_original$Minus2LogLikelihood - sum_sub2_original$Minus2LogLikelihood 
    	
    	p_invariance <- pchisq(lr_invariance, df_invariance, lower.tail=F)
    	
    	if (verbose) {
    	  cat(sum_sub1_original$estimatedParameters ,":", sum_sub2_original$estimatedParameters, ":",sum_sub1$estimatedParameters ,":",sum_sub2$estimatedParameters);
    	  cat(paste(" |-- Invariance result: LR ",lr_invariance,df1,df2,p_invariance,"\n") );
    	}
    	
    	#if (p_invariance <= alpha.level) {
    #		lr <- -1;
    #	}
    	if (is.null(alpha.level)) {
    		alpha.level = 0.05
    	}

    	
    	result <- list();
		#result$alpha.level <- alpha.level;
    	result$passed <- (p_invariance > alpha.level);
    	result$p.value <- p_invariance;

    	#print(result)
    	#print(paste(p_invariance, result$passed));
    	
    	if (is.na(result$passed)) {
    		result$passed <- F
    		warning("Invariance Test produced a NaN value!")	
    	}
    	
    	return(result);
    	
}
