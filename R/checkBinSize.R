checkBinSize <- function(test.result, control, level, covariate, n, mydata, fit,
                         sandwich., p.max, test) {
      
      # sanity check
      if (is.na(test.result$left_n) || is.na(test.result$right_n)) {
        ui_warn("Boundaries NA in checkBinSize()")
        return(test.result)
      }
      
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
          if (nlevels(covariate_up) == 1) { # test this
            test.result$p.value <- 1
            return(test.result)
          }
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
            fit@Options$model.type, '(parTable(fit), data = mydata_up, missing = \'', 
            fit@Options$missing, '\')', sep = "")))), silent = TRUE)
          Scores_up <- lavScores(fit_up)
        }
        # Vcov
        if (identical(control$information.matrix, "info")) {
          vcov_up <- solve(vcov_semtree(fit_up) * n_up)
          vcov_up <- strucchange::root.matrix(vcov_up)
        }
        
        # Re-calcuate cumlative score process
        scus_up <- gefp_semtree(x = fit_up, order.by = covariate_up, vcov = vcov_up,
                                scores = Scores_up, decorrelate = TRUE,
                                sandwich = sandwich., parm = NULL)
        
        # Update functional
        functional_up <- switch(test, dm = strucchange::maxBB,
                                cvm = strucchange::meanL2BB, 
                                suplm = strucchange::supLM(from = control$strucchange.from,
                                                           to = control$strucchange.to),
                                lmuo = strucchange::catL2BB(factor(covariate_up)),
                                wdmo = strucchange::ordwmax(factor(covariate_up)), 
                                maxlmo = strucchange::ordL2BB(factor(covariate_up),
                                                              nproc = NCOL(scus_up$process), 
                                                              nobs = NULL, nrep = control$strucchange.nrep))
        
        # Re-run score test
        test.result <- strucchange::sctest(scus_up, functional = functional_up)
        
        # Check if the re-calculated p-value is still smaller
        if (test.result$p.value > min(c(control$alpha, p.max))) {return(test.result)}
        
        # Re-calculate cutpoint and parameter contributions
        test.result <- c(test.result,
                         sctest_info(CSP = as.matrix(scus_up$process),
                                     covariate = covariate_up,
                                     test = test,
                                     scaled_split = control$scaled_scores,
                                     from = control$strucchange.from,
                                     to = control$strucchange.to))
        
        ## Continue checking right
        if (test.result$right_n < control$min.bucket) {
          if (level == "metric") {
            n_up <- n_up - n_remove
            if (n_up < control$min.N) {
              test.result$p.value <- 1
              return(test.result)
            }
            ids_remove <- seq(from = n_up + 1, to = n_up + n_remove)
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
            if (nlevels(covariate_up) == 1) { # test this
              test.result$p.value <- 1
              return(test.result)
            }
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
              fit@Options$model.type, '(parTable(fit), data = mydata_up, missing = \'', 
              fit@Options$missing, '\')', sep = "")))), silent = TRUE)
            Scores_up <- lavScores(fit_up)
          }
          # vcov
          if (identical(control$information.matrix, "info")) {
            vcov_up <- solve(vcov_semtree(fit_up) * n_up)
            vcov_up <- strucchange::root.matrix(vcov_up)
          }
          # Re-calcuate cumlative score process
          scus_up <- gefp_semtree(x = fit_up, order.by = covariate_up,
                                  vcov = vcov_up, scores = Scores_up,
                                  decorrelate = TRUE, sandwich = sandwich.,
                                  parm = NULL)
          
          functional_up <- switch(test, dm = strucchange::maxBB,
                                  cvm = strucchange::meanL2BB, 
                                  suplm = strucchange::supLM(from = control$strucchange.from,
                                                             to = control$strucchange.to),
                                  lmuo = strucchange::catL2BB(factor(covariate_up)),
                                  wdmo = strucchange::ordwmax(factor(covariate_up)), 
                                  maxlmo = strucchange::ordL2BB(factor(covariate_up),
                                                                nproc = NCOL(scus_up$process), 
                                                                nobs = NULL, nrep = control$strucchange.nrep))
          
          # Re-run score test
          test.result <- strucchange::sctest(scus_up, functional = functional_up)
          
          # Check if the re-calculated p-value is still smaller
          if (test.result$p.value > min(c(control$alpha, p.max))) {return(test.result)}
          
          # Re-calculate cutpoint and parameter contributions
          test.result <- c(test.result,
                           sctest_info(CSP = as.matrix(scus_up$process),
                                       covariate = covariate_up,
                                       test = test,
                                       scaled_split = control$scaled_scores,
                                       from = control$strucchange.from,
                                       to = control$strucchange.to))
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
          ids_remove <- seq(from = n_up + 1, to = n_up + n_remove)
          covariate_up <- covariate[-ids_remove]
          mydata_up <- mydata[-ids_remove, ]
        }
        if (level %in% c("ordinal", "nominal")) {
          ids_remove <- covariate %in% as.numeric(names(
            which(rev(cumsum(rev(table(covariate)))) < control$min.bucket)))
          n_remove <- sum(ids_remove)
          n_up <- n - n_remove
          if (n_up < control$min.N) {
            test.result$p.value <- 1
            return(test.result)
          }
          covariate_up <- covariate[!ids_remove]
          covariate_up <- droplevels(covariate_up)
          if (nlevels(covariate_up) == 1) { # test this
            test.result$p.value <- 1
            return(test.result)
          }
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
            fit@Options$model.type, '(parTable(fit), data = mydata_up, missing = \'', 
            fit@Options$missing, '\')', sep = "")))), silent = TRUE)
          Scores_up <- lavScores(fit_up)
        }
        # vcov
        if (identical(control$information.matrix, "info")) {
          vcov_up <- solve(vcov_semtree(fit_up) * n_up)
          vcov_up <- strucchange::root.matrix(vcov_up)
        }
        
        # Re-calcuate cumlative score process
        scus_up <- gefp_semtree(x = fit_up, order.by = covariate_up, vcov = vcov_up,
                                scores = Scores_up, decorrelate = TRUE,
                                sandwich = sandwich., parm = NULL)
        
        functional_up <- switch(test, dm = strucchange::maxBB,
                                cvm = strucchange::meanL2BB, 
                                suplm = strucchange::supLM(from = control$strucchange.from,
                                                           to = control$strucchange.to),
                                lmuo = strucchange::catL2BB(factor(covariate_up)),
                                wdmo = strucchange::ordwmax(factor(covariate_up)), 
                                maxlmo = strucchange::ordL2BB(factor(covariate_up),
                                                              nproc = NCOL(scus_up$process), 
                                                              nobs = NULL, nrep = control$strucchange.nrep))
        
        # Re-run score test
        test.result <- strucchange::sctest(scus_up, functional = functional_up)
        
        # Check if the re-calculated p-value is still smaller
        if (test.result$p.value > min(c(control$alpha, p.max))) {return(test.result)}
        
        # Re-calculate cutpoint and parameter contributions
        test.result <- c(test.result,
                         sctest_info(CSP = as.matrix(scus_up$process),
                                     covariate = covariate_up,
                                     test = test,
                                     scaled_split = control$scaled_scores,
                                     from = control$strucchange.from,
                                     to = control$strucchange.to))
        
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
              which(cumsum(table(covariate_up)) < control$min.bucket)))
            n_remove <- sum(ids_remove)
            n_up <- n_up - n_remove
            if (n_up < control$min.N) {
              test.result$p.value <- 1
              return(test.result)
            }
            covariate_up <- covariate_up[!ids_remove]
            covariate_up <- droplevels(covariate_up)
            if (nlevels(covariate_up) == 1) { # test this
              test.result$p.value <- 1
              return(test.result)
            }
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
              fit@Options$model.type, '(parTable(fit), data = mydata_up, missing = \'', 
              fit@Options$missing, '\')', sep = "")))), silent = TRUE)
            Scores_up <- lavScores(fit_up)
          }
          # vcov
          if (identical(control$information.matrix, "info")) {
            vcov_up <- solve(vcov_semtree(fit_up) * n_up)
            vcov_up <- strucchange::root.matrix(vcov_up)
          }
          # Re-calcuate cumlative score process
          scus_up <- gefp_semtree(x = fit_up, order.by = covariate_up, 
                                  vcov = vcov_up, scores = Scores_up,
                                  decorrelate = TRUE, sandwich = sandwich.,
                                  parm = NULL)
          
          functional_up <- switch(test, dm = strucchange::maxBB,
                                  cvm = strucchange::meanL2BB, 
                                  suplm = strucchange::supLM(from = control$strucchange.from,
                                                             to = control$strucchange.to),
                                  lmuo = strucchange::catL2BB(factor(covariate_up)),
                                  wdmo = strucchange::ordwmax(factor(covariate_up)), 
                                  maxlmo = strucchange::ordL2BB(factor(covariate_up),
                                                                nproc = NCOL(scus_up$process), 
                                                                nobs = NULL, nrep = control$strucchange.nrep))
          
          # Re-run score test
          test.result <- strucchange::sctest(scus_up, functional = functional_up)
          
          # Check if the re-calculated p-value is still smaller
          if (test.result$p.value > min(c(control$alpha, p.max))) {return(test.result)}
          
          # Re-calculate cutpoint and parameter contributions
          test.result <- c(test.result,
                           sctest_info(CSP = as.matrix(scus_up$process),
                                       covariate = covariate_up,
                                       test = test,
                                       scaled_split = control$scaled_scores,
                                       from = control$strucchange.from,
                                       to = control$strucchange.to))
        }
      }
      
      return(test.result)
    }