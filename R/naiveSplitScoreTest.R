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
  type.max <- NA
  p.max <- 1
  contrib.max <- NA
  btn.matrix <- NULL
  
  # Used for verbose = TRUE and not used outside this function
  # Question: what do these variables refer to?
  level_max <- NA
  test_max <- NA
  
  ### fit model once to complete data
  # OpenMx
  if(control$sem.prog == 'OpenMx'){
    fit <- mxAddNewModelData(model, mydata, name = "BASE MODEL")
    fit <- try(mxRun(fit, silent = TRUE, suppressWarnings = TRUE), silent = TRUE)
  }
  # lavaan
  if(control$sem.prog == 'lavaan'){
    fit <- try(suppressWarnings(eval(parse(text = paste(
      model@Options$model.type, '(parTable(model), data = mydata, missing = \'', 
      model@Options$missing, '\')', sep = "")))), silent = TRUE)
  }
  
  # Number of cases in the node
  n <- lavaan::nobs(fit)
  
  # calculate maximum likelihood scores
  Scores <- sandwich::estfun(fit, control = control)
  
  # get covariance matrix of the model parameters
  if (identical(control$information.matrix, "info")) {
    vcov. <- solve(vcov(fit) * n)
    vcov. <- root.matrix(vcov.)
    sandwich. <- FALSE
  } else {
    sandwich. <- TRUE
  }
  
  
  ############################################
  # main loop with calls to sctest_semtree() #
  ############################################
  for (cur_col in cmp.column.ids) {					   
    
    covariate <- mydata[,cur_col]
    
    # check if covariate has more than one unique value
    if (length(unique(covariate)) > 1) {
      
      cur.name <- colnames(mydata)[cur_col]
      
      # sort scores and covariate
      index <- order(covariate)
      Scores_sorted <- Scores[index, , drop = FALSE]
      covariate <- covariate[index]
      
      # calculate cumulative score process
      scus <- gefp_semtree(x = fit, order.by = covariate, vcov = vcov., 
                           scores = Scores_sorted, decorrelate = TRUE,
                           sandwich = sandwich., parm = NULL)
      
      # defaults
      # TODO: implement semtrees focus parameter interface (AB)
      parameter <- NULL
      
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
      
      # get test statstic object
      functional <- switch(test, dm = maxBB,
                           cvm = meanL2BB, 
                           suplm = supLM(from = 0.1, to = NULL), # give option?
                           lmuo = catL2BB(factor(covariate)),
                           wdmo = ordwmax(factor(covariate)), 
                           maxlmo = ordL2BB(factor(covariate), nproc = NCOL(scus$process), 
                                            nobs = NULL, nrep = 50000),
                           stop("Unknown efp functional. Use: LMuo (categorical); wdmo or maxLMo (ordinal); DM, supLM, or CvM (metric)."))
      
      # peform score test
      test.result <- sctest(scus, functional = functional)
      
      # get cutpoint and parameter contributions
      if (test.result$p.value < min(c(control$alpha, p.max))) { # only if current p-value is smaller than other p-values
        test.result <- c(test.result,
                         sctest_info(CSP = as.matrix(scus$process),
                                 covariate = covariate,
                                 test = test,
                                 scaled_split = control$scaled_scores,
                                 from = 0.1,
                                 to = NULL))
        
        # check if cutpoint is too close to the border
        if (!(cur.type == 1 & nlevels(covariate) > 2)) { # do not do that categorical covariate with more than two levels
          test.result <- checkBinSize(test.result = test.result,
                                      control = control,
                                      level = level,
                                      covariate = covariate,
                                      n = n,
                                      mydata = mydata,
                                      fit = fit,
                                      sandwich. = sandwich.,
                                      functional = functional,
                                      p.max = p.max,
                                      test = test)
        }
      }
    
      # Standardise output
      ts <- test.result$statistic
      splt <- test.result$cutpoint
      pval <- test.result$p.value
      contrib <- test.result$par.contrib
      
      if (control$verbose) {
        cat("Testing:", cur.name, "\n")
        cat("Level of measurement:", level, "\n")
        cat(test, " test statistic: ", ts, ", p-value: ", pval, "\n", sep = "")
        cat("Best so far: ",name.max, " (", level_max, "), ", test_max, ": ",
            ts, ", p-value: ", p.max, " split point: ", splt)
      }
      
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
    }
  }

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
  
  # format results
  return(list(LL.max=LL.max,split.max=split.max,name.max=name.max,
              col.max=col.max, type.max=type.max, n.comp=n.comp,
              btn.matrix = btn.matrix, 
              invariance.filter=NULL, p.max = p.max, contrib.max=contrib.max))
}
