naiveSplitScoreTest <- function(model = NULL, mydata = NULL, control = NULL, 
                                invariance = NULL, meta = NULL, 
                                pp = FALSE, constraints = NULL, ...) {
  
  # TODO
  # - test for invariance
  # - test for constraints
  # - MA: is there a function outside this function that checks if the sample is
  #  large enough to be split? # MA: No, there is not.
  
  if(pp) {cmp.column.ids <- max(meta$model.ids+1)}
  else {cmp.column.ids <- meta$covariate.ids}
  
  LL.max <- -Inf
  split.max <- NA
  col.max <- NA
  name.max <- NA
  type.max <- 99 # This cannot be NA (gives an error if all covariates lead to too small bins)
  p.max <- 1
  contrib.max <- NA
  btn.matrix <- NULL
  
  # Used for verbose = TRUE and not used outside this function
  # Question: what do these variables refer to?
  level_max <- NA
  test_max <- NA
  
  ### fit model once to complete data and compute maximum likelihood scores
  # OpenMx
  if(control$sem.prog == 'OpenMx'){
    fit <- mxAddNewModelData(model, mydata, name = "BASE MODEL")
    fit <- try(OpenMx::mxRun(fit, silent = TRUE, suppressWarnings = TRUE), silent = TRUE)
    ### Check for error and abort
    Scores <- mxScores(fit, control = control)
  }
  # lavaan
  if(control$sem.prog == 'lavaan'){
    fit <- try(suppressWarnings(eval(parse(text = paste(
      model@Options$model.type, '(parTable(model), data = mydata, missing = \'', 
      model@Options$missing, '\')', sep = "")))), silent = TRUE)
    ### Check for error and abort
    Scores <- lavScores(fit)
  }
  
  # Number of cases in the node
  n <- nobs(fit)
  
  # get covariance matrix of the model parameters
  if (identical(control$information.matrix, "info")) {
    vcov. <- solve(vcov_semtree(fit) * n)
    vcov. <- strucchange::root.matrix(vcov.)
    sandwich. <- FALSE
  } else {
    sandwich. <- TRUE
  }
  
  
  ############################################
  # main loop with calls to sctest_semtree() #
  # each iteration evaluates a covariate     #
  ############################################
  for (cur_col in cmp.column.ids) {					   
    
    covariate <- mydata[,cur_col]
    # check if covariate has more than one unique value
    if (length(unique(covariate)) > 1) {
      
      cur.name <- colnames(mydata)[cur_col]
      
      # sort scores, model data, and covariate
      index <- order(covariate)
      Scores_sorted <- Scores[index, , drop = FALSE]
      mydata_sorted <- mydata[index, , drop = FALSE]
      covariate_sorted <- covariate[index]
      
      # calculate cumulative score process
      scus <- gefp_semtree(x = fit, order.by = covariate_sorted, vcov = vcov., 
                           scores = Scores_sorted, decorrelate = TRUE,
                           sandwich = sandwich., parm = constraints$focus.parameters)
      
      # Level of measurement and test statistic
      if (!is.factor(covariate_sorted)) {
        level <- "metric"
        test <- control$score.tests["metric"][[1]]  # default: supLM
        cur.type <- 2
      } else {
        cov_levels <- nlevels(x = covariate_sorted)
        if (is.ordered(covariate_sorted)) {
          level <- "ordinal"
          test <- control$score.tests["ordinal"][[1]] # default: "maxLMo"
          cur.type <- 2
        } else {
          level <- "nominal"
          test <- control$score.tests["nominal"][[1]] # default: "LM" 
          cur.type <- 1
        }
      }
      
      # get test statstic object
      functional <- switch(test, dm = strucchange::maxBB,
                           cvm = strucchange::meanL2BB, 
                           suplm = strucchange::supLM(from = control$strucchange.from, to = control$strucchange.to),
                           lmuo = strucchange::catL2BB(factor(covariate_sorted)),
                           wdmo = strucchange::ordwmax(factor(covariate_sorted)), 
                           maxlmo = strucchange::ordL2BB(factor(covariate_sorted), nproc = NCOL(scus$process), 
                                            nobs = NULL, nrep = control$strucchange.nrep),
                           stop("Unknown efp functional. Use: LMuo (categorical); wdmo or maxLMo (ordinal); DM, maxLM (alias: supLM), or CvM (metric)."))
      
      # peform score test
      test.result <- strucchange::sctest(scus, functional = functional)
      
      
      # get cutpoint and parameter contributions
      if (test.result$p.value < min(c(control$alpha, p.max))) { # only if current p-value is smaller than other p-values
        test.result <- c(test.result,
                         sctest_info(CSP = as.matrix(scus$process),
                                     covariate = covariate_sorted,
                                     test = test,
                                     scaled_split = control$scaled_scores,
                                     from = control$strucchange.from,
                                     to = control$strucchange.to))
        
        
        # check if cutpoint is too close to the border
        if (!(cur.type == 1 & nlevels(covariate_sorted) > 2)) { # do not use with categorical covariates with more than two levels
          test.result <- checkBinSize(test.result = test.result,
                                      control = control,
                                      level = level,
                                      covariate = covariate_sorted,
                                      n = n,
                                      mydata = mydata_sorted,
                                      fit = fit,
                                      sandwich. = sandwich.,
                                      p.max = p.max,
                                      test = test)
          
        }
      } else {
        test.result$cutpoint <- NA
      }
      
      # Standardise output
      ts <- test.result$statistic
      pval <- test.result$p.value
      splt <- test.result$cutpoint
      contrib <- test.result$par.contrib
      
      if (pval < p.max) { # Use p values to compare covariates
        LL.max <- ts
        split.max <- splt
        col.max <- cur_col
        name.max <- cur.name
        type.max <- cur.type
        p.max <- pval
        contrib.max <- contrib
        level_max <- NA # not used outside this function
        test_max <- NA # not used outside this function
      }
      
      if (control$verbose) {
        cat("Score test of :", cur.name, " (")
        cat("Level of measurement:", level, ")\n")
        cat("     |--- ",test, ", test statistic: ", ts, ", p-value: ", pval, "\n", sep = "")
        cat("     |--- Best so far: ", name.max, " (", level_max, "), ", test_max, ": ",
            LL.max, ", p-value: ", p.max, " split point: ", split.max,"\n")
        cat("     |--- ",type.max,"\n")
      }
    }
  }
  
  #######################
  # main loop ends here #
  #######################
  
  # Call naiveSplit to get cutpoints for categorical covariates with more than two levels
  if (p.max < 1) {
    if (type.max == 1 & nlevels(mydata[, col.max]) > 2) {
      if (control$sem.prog == 'OpenMx') {
        mydata <- mydata[, c(fit$manifestVars, name.max)]
      }
      if (control$sem.prog == 'lavaan') {
        mydata <- mydata[, c(fit@pta$vnames$ov[[1]], name.max)]
      }
      meta$covariate.ids <- NCOL(mydata)
      meta$model.ids <- seq_len(meta$covariate.ids - 1)
      test.results <- naiveSplit(model, mydata, control, invariance, meta,
                                 constraints = constraints)
      split.max <- test.results$split.max
      btn.matrix <- test.results$btn.matrix
    }
  }
  
  n.comp <- length(cmp.column.ids)
  
  return(list(LL.max = LL.max, split.max = split.max, name.max = name.max,
              col.max = col.max, type.max = type.max, n.comp = n.comp,
              btn.matrix = btn.matrix, 
              invariance.filter = NULL, p.max = p.max, contrib.max = contrib.max))
}
