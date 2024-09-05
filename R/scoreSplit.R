ScoreSplit <- function(model = NULL, mydata = NULL, control = NULL,
                       invariance = NULL, meta = NULL,  pp = FALSE,
                       constraints = NULL, ...) {
  
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
  LL.ratio.max <- -Inf
  contrib.max <- NA
  btn.matrix <- NULL
  par.contrib <- NA
  
  # Used for verbose = TRUE and not used outside this function
  # Question: what do these variables refer to?
  level_max <- NA
  test_max <- NA
  
  # Compute scores
  Scores <- switch(control$sem.prog,
                   "OpenMx" = mxScores(model, control = control),
                   "lavaan" = lavScores(model),
                   "ctsem" = ctsemScores(model))
  
  # Number of cases in the node
  n_obs <- nobs(model)
  
  # get covariance matrix of the model parameters
  vcov. <- tryCatch({
     solve(vcov_semtree(model) * n_obs)
  }, error=function(e){
    ui_fail("An error occured inverting the vcov model matrix when computing scores! Nobs=",n_obs," Aborting.")
     NULL
  })
  
  if (is.null(vcov.)) {
    return(NULL) 
  }
  
  vcov. <- strucchange::root.matrix(vcov.)
  
  ############################################
  # main loop with calls to sctest_semtree() #
  # each iteration evaluates a covariate     #
  ############################################
  for (cur_col in cmp.column.ids) {
    
    covariate <- mydata[,cur_col]
    n_unique_cov_values <- length(unique(covariate))
    # check if covariate has more than one unique value
    if (n_unique_cov_values > 1) {
      
      cur.name <- colnames(mydata)[cur_col]
      
      # sort scores, model data, and covariate
      index <- order(covariate)
      Scores_sorted <- Scores[index, , drop = FALSE]
      mydata_sorted <- mydata[index, , drop = FALSE]
      covariate_sorted <- covariate[index]
      
      # calculate cumulative score process
      scus <- gefp_semtree(x = model, order.by = covariate_sorted, vcov = vcov.,
                           scores = Scores_sorted, decorrelate = TRUE,
                           sandwich = FALSE,
                           parm = constraints$focus.parameters)
      
      # Level of measurement and test statistic
      if (!is.factor(covariate_sorted)) {
        level <- "metric"
        cur.type <- .SCALE_METRIC
        cur.test <- "maxLM"
      } else {
        cov_levels <- nlevels(x = covariate_sorted)
        if (is.ordered(covariate_sorted)) {
          level <- "ordinal"
          cur.type <- .SCALE_ORDINAL
          cur.test <- "maxLMo"
        } else {
          if (n_unique_cov_values == 2) {
            level <- "dummy"
          } else {
            level <- "multinomial"
          }
          cur.type <- .SCALE_CATEGORICAL
          cur.test <- "LM"
        }
      }
      
      # peform score test
      test.results <- switch(level,
                             "metric" = sctest_continuous(
                               cov_sort = covariate_sorted,
                               scus = scus,
                               from = control$strucchange.from,
                               to = control$strucchange.to,
                               min.bucket = control$min.bucket),
                             "ordinal" = sctest_ordinal(
                               cov_sort = covariate_sorted,
                               scus = scus,
                               nrep = control$strucchange.nrep,
                               min.bucket = control$min.bucket),
                             "multinomial" = sctest_multinomial(
                               covariate = covariate,
                               cov_name = cur.name,
                               min.bucket = control$min.bucket,
                               model = model,
                               vcov = vcov.,
                               scores = Scores,
                               parm = constraints$focus.parameters,
                               cur_col = cur_col,
                               cov_sort = covariate_sorted,
                               scus = scus),
                             "dummy" = sctest_dummy(
                               cov_sort = covariate_sorted,
                               scus = scus,
                               min.bucket = control$min.bucket
                             ))
      
      # 26.08.2022: Perform likelihood ratio test if p = 0
      if (test.results$p.value == 0) {
        LL.baseline <- minus2logLik_from_fitted_models(model)
        if (level == "multinomial") {
          subset1 <- mydata[!test.results$best_partition, -meta$covariate.ids,
                            drop = FALSE]
          subset2 <- mydata[test.results$best_partition, -meta$covariate.ids,
                            drop = FALSE]
        } else {
          subset1 <- mydata_sorted[mydata_sorted[, cur_col] <= test.results$cutpoint,
                                   -meta$covariate.ids, drop = FALSE]
          subset2 <- mydata_sorted[mydata_sorted[, cur_col] > test.results$cutpoint, 
                                   -meta$covariate.ids, drop = FALSE]
        }
        LL.return <- fitSubmodels(model = model, subset1 = subset1,
                                  subset2 = subset2, control = control,
                                  invariance = invariance)
        LL.ratio <- LL.baseline - LL.return
        test.results$p.value <- pchisq(q = LL.ratio[1], df = scus$nreg, # check scus$nreg
                                       lower.tail = FALSE)
      } else {
        LL.ratio <- NA
      }
      
      
      # Standardise output
      ts <- test.results$statistic
      pval <- test.results$p.value
      splt <- test.results$cutpoint
      cur.par.contrib <- test.results$par.contrib
      btn <- test.results$btn.matrix
      
      # 18.08.2022: compare LL_ratio if p-values are zero
      if (pval < p.max | (pval == 0 & LL.ratio > LL.ratio.max)) {
        LL.max <- ts
        split.max <- splt
        col.max <- cur_col
        name.max <- cur.name
        type.max <- cur.type
        btn.matrix <- btn
        p.max <- pval
        if (!is.na(LL.ratio)) {LL.ratio.max <- LL.ratio}
        par.contrib <- cur.par.contrib
        level_max <-  level
        test_max <- cur.test
      }
      
      if (control$verbose) {
        cat("Score test of :", cur.name, " (")
        cat("Level of measurement:", level, ")\n")
        cat("     |--- ",cur.test, ", test statistic: ", ts, ", p-value: ", pval, "\n", sep = "")
        cat("     |--- Best so far: ", name.max, " (", level_max, "), ", test_max, ": ",
            LL.max, ", p-value: ", p.max, " split point: ", split.max,"\n")
        cat("     |--- ",type.max,"\n")
      }
    }
  }
  
  
  #######################
  # main loop ends here #
  #######################
  
  if ((is.null(split.max)) || (is.na(split.max))) {
    if (control$report.level >= 50) cat("Split.max is null or NA!\n")
    p.max=1
    LL.max=0
  }
  
  n.comp <- length(cmp.column.ids)
  
  return(list(LL.max = LL.max, split.max = split.max, name.max = name.max,
              col.max = col.max, type.max = type.max, n.comp = n.comp,
              btn.matrix = btn.matrix, invariance.filter = NULL, p.max = p.max,
              LL.ratio.max = LL.ratio.max, par.contrib = par.contrib))
}
