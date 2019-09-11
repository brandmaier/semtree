naiveSplitScoreTest <- function(model=NULL, mydata=NULL, control=NULL, 
                                invariance=NULL, meta=NULL, 
                                pp=FALSE, constraints=NULL, ...) {
  
  if(control$sem.prog != 'OpenMx'){ stop("Score Test is not implemented for lavaan models yet.") }
  
  # TODO
  # - test for invariance
  # - test for constraints
  # - MA: is there a function outside this function that checks if the sample is
  #  large enough to be split?
  
  if(pp) {cmp.column.ids <- max(meta$model.ids+1)}
  else {cmp.column.ids <- meta$covariate.ids}
  
  LL.max <- -Inf
  split.max <- NA
  col.max <- NA
  name.max <- NA
  type.max <- NA
  p.max <- 1
  contrib.max <- NA
  
  # Used for verbose = TRUE and not used outside this function
  level_max <- NA
  test_max <- NA
  
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
    
  #  print("Call")
    
    
    ##################################
    ### Changes from MA start here ###
    ##################################
    
    # Sort data and covariate with respect to covariate
    covariate_sorted <- covariate[order(covariate)]
    data_sorted <- as.matrix(fit$data$observed[, fit$manifestVars,drop = FALSE])
    data_sorted <- data_sorted[order(covariate), ]
    
    # Level of measurement and test statistic
    if (!is.factor(covariate)) {
      level <- "metric"
      test <- control$score.tests["metric"][[1]]  # default: CvM
      cur.type <- 2
    } else {
      cov_levels <- nlevels(x = covariate)
      if (is.ordered(covariate)) {
        level <- "ordinal"
        test <- control$score.tests["ordinal"][[1]] # default: "maxLM"
        cur.type <- 2
      } else {
        level <- "nominal"
        test <- control$score.tests["nominal"][[1]] # default: "LM" 
        cur.type <- 1
      }
    }
    
    # Control object for the bin size
    bin_control <- list(small_bin = FALSE, censored_left = FALSE,
                        censored_right = FALSE)
    
    # main call to score test
    test.result <- scoretest(fit = fit,
                             data_sorted = data_sorted,
                             covariate_sorted = covariate_sorted,
                             level = level,
                             test = test,
                             alpha = control$alpha,
                             min_bucket = control$min.bucket,
                             bin_control = bin_control)
    
    # TODO AB: If the covariate is nominal and has three or more leves, use the
    # likelihood ratio semtree implementation to determine cut point on
    # covariate. Test statistic and p-value can be taken from test.result above.
    
    # Re-calculates scoretest if the cut point is too close to the boundary
    if (!identical(test.result$bin_control$small_bin, FALSE)) {
      # First re-run
      test.result <- scoreTestcensored(fit = fit, 
                                       data_sorted = data_sorted, 
                                       covariate_sorted = covariate_sorted,
                                       level = level,
                                       test = test,
                                       alpha = control$alpha,
                                       min_bucket = control$min.bucket,
                                       bin_control = test.result$bin_control)
      # Second re-run
      # Re-calculates scoretest if the cut point is too close to the boundary
      if (!identical(test.result$test_result$bin_control$small_bin, FALSE)) {
        test.result <- scoreTestcensored(fit = fit, 
                                         data_sorted = test.result$data_sorted, 
                                         covariate_sorted = test.result$covariate_sorted,
                                         level = level,
                                         test = test,
                                         alpha = control$alpha,
                                         min_bucket = control$min.bucket,
                                         bin_control = test.result$test_result$bin_control)
      }
      # Get standard output back
      test.result <- test.result$test_result
    }
      
      
      #######TODO Für einhetlichen Output
      ts <- test.result$`Test statistic`
    splt <- test.result$`Cut point`
    pval <- test.result$`p-value`
    
    
    if (control$verbose) {
      cat("Testing:", cur.name, "\n")
      cat("Level of measurement:", level, "\n")
      cat(test, " test statistic: ", ts, ", p-value: ", pval, "\n", sep = "")
      cat("Best so far: ",name.max, " (", level_max, "), ", test_max, ": ",
          ts, ", p-value: ", p.max, " split point: ", splt)
    }
    
    if (pval < p.max) { # changed too p-value
      LL.max <- ts
      split.max <- splt
      col.max <- cur_col
      name.max <- cur.name
      type.max <- cur.type
      p.max <- pval
      contrib.max <- test.result$`Parameter contribution`
      level_max <- NA # not used outside this function
      test_max <- NA # not used outside this function
    }
  }
  
  n.comp <- length(cmp.column.ids)
  
  #print(list(LL.max=LL.max,split.max=split.max,name.max=name.max,
  #           col.max=col.max, type.max=type.max, n.comp=n.comp, btn.matrix=NULL, 
   #          invariance.filter=NULL, p.max = p.max, contrib.max=contrib.max))
  
  # format results
  return(list(LL.max=LL.max,split.max=split.max,name.max=name.max,
              col.max=col.max, type.max=type.max, n.comp=n.comp, btn.matrix=NULL, 
              invariance.filter=NULL, p.max = p.max, contrib.max=contrib.max))
  
}
