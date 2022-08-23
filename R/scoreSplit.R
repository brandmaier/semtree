scoreSplit <- function(model = NULL, mydata = NULL, control = NULL,
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

  ## 11.08.2022: removed re-estimation. This is already done in growTree
  ### fit model once to complete data and compute maximum likelihood scores
  # # OpenMx
  # if(control$sem.prog == 'OpenMx'){
  #   fit <- mxAddNewModelData(model, mydata, name = "BASE MODEL")
  #   fit <- try(OpenMx::mxRun(fit, silent = TRUE, suppressWarnings = TRUE), silent = TRUE)
  #   ### Check for error and abort
  #   Scores <- mxScores(fit, control = control)
  # }
  # # lavaan
  # if(control$sem.prog == 'lavaan'){
  #   fit <- try(suppressWarnings(eval(parse(text = paste(
  #     model@Options$model.type, '(parTable(model), data = mydata, missing = \'',
  #     model@Options$missing, '\')', sep = "")))), silent = TRUE)
  #   ### Check for error and abort
  #   Scores <- lavScores(fit)
  # }
  # ## 26.06.2022: Added code for ctsem models
  # # ctsem
  # if (control$sem.prog == 'ctsem') {
  #   fit <- suppressMessages(try(
  #     ctsemOMX::ctFit(dat = mydata[, -meta$covariate.ids],
  #                     ctmodelobj = model$ctmodelobj,
  #                     dataform = "wide",
  #                     stationary = "all",
  #                     retryattempts = 20)
  #   ))
  #   fit$mxobj@name <- "BASE MODEL"
  #   Scores <- ctsemScores(fit)
  # }

  # Compute scores
  Scores <- switch(control$sem.prog,
                   "OpenMx" = mxScores(model, control = control),
                   "lavaan" = lavScores(model),
                   "ctsem" = ctsemScores(model))

  # Number of cases in the node
  n <- nobs(model)

  # get covariance matrix of the model parameters
  if (identical(control$information.matrix, "info")) {
    vcov. <- solve(vcov_semtree(model) * n)
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
      scus <- gefp_semtree(x = model, order.by = covariate_sorted, vcov = vcov.,
                           scores = Scores_sorted, decorrelate = TRUE,
                           sandwich = sandwich.,
                           parm = constraints$focus.parameters)

      # Level of measurement and test statistic
      if (!is.factor(covariate_sorted)) {
        level <- "metric"
        test <- control$score.tests["metric"][[1]]  # default: supLM
        cur.type <- .SCALE_METRIC
      } else {
        cov_levels <- nlevels(x = covariate_sorted)
        if (is.ordered(covariate_sorted)) {
          level <- "ordinal"
          test <- control$score.tests["ordinal"][[1]] # default: "maxLMo"
          cur.type <- .SCALE_ORDINAL
        } else {
          level <- "nominal"
          test <- control$score.tests["nominal"][[1]] # default: "LM"
          cur.type <- .SCALE_CATEGORICAL
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
                            "nominal" = sctest_nominal(
                              cov_sort = covariate_sorted,
                              scus = scus,
                              min.bucket = control$min.bucket
                            ))

      # locate cutpoint on nominal predictors with more than two levels
      if (test.results$cutpoint == "naive split" &
          ((test.results$p.value == 0 & test.results$statistic > LL.ratio.max) |
           test.results$p.value < p.max)) {
        if (control$sem.prog == 'OpenMx') {
          mydata_cat <- mydata[, c(model$manifestVars, cur.name)]
        }
        if (control$sem.prog == 'lavaan') {
          mydata_cat <- mydata[, c(model@pta$vnames$ov[[1]], cur.name)]
        }
        meta$covariate.ids <- NCOL(mydata_cat)
        meta$model.ids <- seq_len(meta$covariate.ids - 1)
        naive.test.results <- naiveSplit(model, mydata_cat, control, invariance, meta,
                                   constraints = constraints)
        test.results$cutpoint <- naive.test.results$split.max
        btn.matrix <- naive.test.results$btn.matrix
        if (is.null(naive.test.results)) {test.results$p.value <- 1}
      } else {
        btn.matrix <- NA
      }

      # 18.08.2022: Get likelihood ratio if p = 0
      if (test.results$p.value == 0 &
          cur.type %in% c(.SCALE_METRIC, .SCALE_ORDINAL)) {
        LL.baseline <- minus2logLik_from_fitted_models(model)
        subset1 <- mydata_sorted[mydata_sorted[, cur_col] <= test.results$cutpoint,
                                 -meta$covariate.ids]
        subset2 <- mydata_sorted[mydata_sorted[, cur_col] > test.results$cutpoint, 
                                 -meta$covariate.ids]
        LL.return <- fitSubmodels(model = model, subset1 = subset1,
                                  subset2 = subset2, control = control,
                                  invariance = invariance)
        LL.ratio <- LL.baseline - LL.return
      } else {
        LL.ratio <- -Inf
      }


      # Standardise output
      ts <- test.results$statistic
      pval <- test.results$p.value
      splt <- test.results$cutpoint
      cur.par.contrib <- test.results$par.contrib

      # 18.08.2022: compare LL_ratio if p-values are zero
      if ((pval == 0 & ts > LL.ratio.max) | pval < p.max) {
        LL.max <- ts
        split.max <- splt
        col.max <- cur_col
        name.max <- cur.name
        type.max <- cur.type
        btn.matrix <- btn.matrix
        p.max <- pval
        if (!is.na(ts)) {LL.ratio.max <- ts}
        par.contrib <- cur.par.contrib
        level_max <-  level
        test_max <- test
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
