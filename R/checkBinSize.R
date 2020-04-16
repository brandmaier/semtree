checkBinSize <- function(test.result, control, level, covariate, n, mydata,
                         fit, sandwich., functional, p.max, test) {
  
  ### Start checking left
  if (test.result$left_n < control$min.bucket) {
    if (level == "metric") {
      n_remove <- control$min.bucket - 1
      n_up <- n - n_remove
      if (n_up < control$min.N) {
        test.result$p.value <- 1
        return(test.result)
      }
      ids_remove <- seq_len(n_remove)
      covariate_up <- covariate[-ids_remove]
      mydata_up <- mydata[-ids_remove, ]
    }
    if (level %in% c("ordinal", "nominal")) {
      ids_remove <- covariate %in% as.numeric(names(
        which(cumsum(table(covariate)) < control$min.bucket)))
      n_remove <- sum(ids_remove)
      n_up <- n - n_remove
      if (n_up < control$min.N) {
        test.result$p.value <- 1
        return(test.result)
      }
      covariate_up <- covariate[!ids_remove]
      covariate_up <- droplevels(covariate_up)
      mydata_up <- mydata[!ids_remove, ]
    }
    if(control$sem.prog == 'OpenMx'){
      fit_up <- mxAddNewModelData(fit, mydata_up, name = "BASE MODEL")
      fit_up <- try(mxRun(fit_up, silent = TRUE, suppressWarnings = TRUE),
                    silent = TRUE)
      Scores_up <- mxScores(fit_up, control = control)
    }
    if(control$sem.prog == 'lavaan'){
      fit_up <- try(suppressWarnings(eval(parse(text = paste(
        fit@Options$model.type, '(parTable(model), data = mydata_up, missing = \'', 
        fit@Options$missing, '\')', sep = "")))), silent = TRUE)
      Scores_up <- lavScores(fit_up)
    }
    # Vcov
    if (identical(control$information.matrix, "info")) {
      vcov_up <- solve(vcov(fit_up) * n_up)
      vcov_up <- root.matrix(vcov_up)
    }
    
    # Re-calcuate cumlative score process
    scus_up <- gefp_semtree(x = fit_up, order.by = covariate_up, vcov = vcov_up,
                            scores = Scores_up, decorrelate = TRUE,
                            sandwich = sandwich., parm = NULL)
    
    # Re-run score test
    test.result <- sctest(scus_up, functional = functional)
    
    # Check if the re-calculated p-value is still smaller
    if (test.result$p.value > min(c(control$alpha, p.max))) {return(test.result)}
    
    # Re-calculate cutpoint and parameter contributions
    test.result <- c(test.result,
                     sctest_info(CSP = as.matrix(scus_up$process),
                                 covariate = covariate_up,
                                 test = test,
                                 scaled_split = control$scaled_scores,
                                 from = 0.1,
                                 to = NULL))
    
    ## Continue checking right
    if (test.result$right_n < control$min.bucket) {
      if (level == "metric") {
        n_up <- n_up - n_remove
        if (n_up < control$min.N) {
          test.result$p.value <- 1
          return(test.result)
        }
        ids_remove <- seq(from = n_up, to = n_up + n_remove)
        covariate_up <- covariate_up[-ids_remove]
        mydata_up <- mydata_up[-ids_remove, ]
      }
      if (level %in% c("ordinal", "nominal")) {
        ids_remove <- covariate_up %in% as.numeric(names(
          which(rev(cumsum(rev(table(covariate_up)))) < control$min.bucket)))
        n_remove <- sum(ids_remove)
        n_up <- n_up - n_remove
        if (n_up < control$min.N) {
          test.result$p.value <- 1
          return(test.result)
        }
        covariate_up <- covariate_up[!ids_remove]
        covariate_up <- droplevels(covariate_up)
        mydata_up <- mydata_up[!ids_remove, ]
      }
      if(control$sem.prog == 'OpenMx'){
        fit_up <- mxAddNewModelData(fit, mydata_up, name = "BASE MODEL")
        fit_up <- try(mxRun(fit_up, silent = TRUE, suppressWarnings = TRUE),
                      silent = TRUE)
        Scores_up <- mxScores(fit_up, control = control)
      }
      if(control$sem.prog == 'lavaan'){
        fit_up <- try(suppressWarnings(eval(parse(text = paste(
          fit@Options$model.type, '(parTable(model), data = mydata_up, missing = \'', 
          fit@Options$missing, '\')', sep = "")))), silent = TRUE)
        Scores_up <- lavScores(fit_up)
      }
      # vcov
      if (identical(control$information.matrix, "info")) {
        vcov_up <- solve(vcov(fit_up) * n_up)
        vcov_up <- root.matrix(vcov_up)
      }
      # Re-calcuate cumlative score process
      scus_up <- gefp_semtree(x = fit_up, order.by = covariate_up,
                              vcov = vcov_up, scores = Scores_up,
                              decorrelate = TRUE, sandwich = sandwich.,
                              parm = NULL)
      
      # Re-run score test
      test.result <- sctest(scus_up, functional = functional)
      
      # Check if the re-calculated p-value is still smaller
      if (test.result$p.value > min(c(control$alpha, p.max))) {return(test.result)}
      
      # Re-calculate cutpoint and parameter contributions
      test.result <- c(test.result,
                       sctest_info(CSP = as.matrix(scus_up$process),
                                   covariate = covariate_up,
                                   test = test,
                                   scaled_split = control$scaled_scores,
                                   from = 0.1,
                                   to = NULL))
    }
  }
  
  
  ### Start checking right
  if (test.result$right_n < control$min.bucket) {
    if (level == "metric") {
      n_remove <- control$min.bucket - 1
      n_up <- n - n_remove
      if (n_up < control$min.N) {
        test.result$p.value <- 1
        return(test.result)
      }
      ids_remove <- seq(from = n_up, to = n_up + n_remove)
      covariate_up <- covariate[-ids_remove]
      mydata_up <- mydata[-ids_remove, ]
    }
    if (level %in% c("ordinal", "nominal")) {
      ids_remove <- covariate %in% as.numeric(names(
        which(cumsum(table(covariate)) < control$min.bucket)))
      n_remove <- sum(ids_remove)
      n_up <- n - n_remove
      if (n_up < control$min.N) {
        test.result$p.value <- 1
        return(test.result)
      }
      covariate_up <- covariate[!ids_remove]
      covariate_up <- droplevels(covariate_up)
      mydata_up <- mydata[!ids_remove, ]
    }
    if(control$sem.prog == 'OpenMx'){
      fit_up <- mxAddNewModelData(fit, mydata_up, name = "BASE MODEL")
      fit_up <- try(mxRun(fit_up, silent = TRUE, suppressWarnings = TRUE),
                    silent = TRUE)
      Scores_up <- mxScores(fit_up, control = control)
    }
    if(control$sem.prog == 'lavaan'){
      fit_up <- try(suppressWarnings(eval(parse(text = paste(
        fit@Options$model.type, '(parTable(model), data = mydata_up, missing = \'', 
        fit@Options$missing, '\')', sep = "")))), silent = TRUE)
      Scores_up <- lavScores(fit_up)
    }
    # vcov
    if (identical(control$information.matrix, "info")) {
      vcov_up <- solve(vcov(fit_up) * n_up)
      vcov_up <- root.matrix(vcov_up)
    }
    
    # Re-calcuate cumlative score process
    scus_up <- gefp_semtree(x = fit_up, order.by = covariate_up, vcov = vcov_up,
                            scores = Scores_up, decorrelate = TRUE,
                            sandwich = sandwich., parm = NULL)
    
    # Re-run score test
    test.result <- sctest(scus_up, functional = functional)
    
    # Check if the re-calculated p-value is still smaller
    if (test.result$p.value > min(c(control$alpha, p.max))) {return(test.result)}
    
    # Re-calculate cutpoint and parameter contributions
    test.result <- c(test.result,
                     sctest_info(CSP = as.matrix(scus_up$process),
                                 covariate = covariate_up,
                                 test = test,
                                 scaled_split = control$scaled_scores,
                                 from = 0.1,
                                 to = NULL))
    
    ## Continue checking left
    if (test.result$left_n < control$min.bucket) {
      if (level == "metric") {
        n_up <- n_up - n_remove
        if (n_up < control$min.N) {
          test.result$p.value <- 1
          return(test.result)
        }
        ids_remove <- seq_len(n_remove)
        covariate_up <- covariate_up[-ids_remove]
        mydata_up <- mydata_up[-ids_remove, ]
      }
      if (level %in% c("ordinal", "nominal")) {
        ids_remove <- covariate_up %in% as.numeric(names(
          which(rev(cumsum(rev(table(covariate_up)))) < control$min.bucket)))
        n_remove <- sum(ids_remove)
        n_up <- n_up - n_remove
        if (n_up < control$min.N) {
          test.result$p.value <- 1
          return(test.result)
        }
        covariate_up <- covariate_up[!ids_remove]
        covariate_up <- droplevels(covariate_up)
        mydata_up <- mydata_up[!ids_remove, ]
      }
      if(control$sem.prog == 'OpenMx'){
        fit_up <- mxAddNewModelData(fit, mydata_up, name = "BASE MODEL")
        fit_up <- try(mxRun(fit_up, silent = TRUE, suppressWarnings = TRUE),
                      silent = TRUE)
        Scores_up <- mxScores(fit_up, control = control)
      }
      if(control$sem.prog == 'lavaan'){
        fit_up <- try(suppressWarnings(eval(parse(text = paste(
          fit@Options$model.type, '(parTable(model), data = mydata_up, missing = \'', 
          fit@Options$missing, '\')', sep = "")))), silent = TRUE)
        Scores_up <- lavScores(fit_up)
      }
      # vcov
      if (identical(control$information.matrix, "info")) {
        vcov_up <- solve(vcov(fit_up) * n_up)
        vcov_up <- root.matrix(vcov_up)
      }
      # Re-calcuate cumlative score process
      scus_up <- gefp_semtree(x = fit_up, order.by = covariate_up, 
                              vcov = vcov_up, scores = Scores_up,
                              decorrelate = TRUE, sandwich = sandwich.,
                              parm = NULL)
      
      # Re-run score test
      test.result <- sctest(scus_up, functional = functional)
      
      # Check if the re-calculated p-value is still smaller
      if (test.result$p.value > min(c(control$alpha, p.max))) {return(test.result)}
      
      # Re-calculate cutpoint and parameter contributions
      test.result <- c(test.result,
                       sctest_info(CSP = as.matrix(scus_up$process),
                                   covariate = covariate_up,
                                   test = test,
                                   scaled_split = control$scaled_scores,
                                   from = 0.1,
                                   to = NULL))
    }
  }
  
  return(test.result)
}