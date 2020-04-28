fairSplit <-
  function(model = NULL,
           mydata = NULL,
           control = NULL,
           invariance = NULL,
           meta = NULL,
           constraints = NULL,
           ...) {
    #browser()
    # add a column of uniform random numbers to the data and
    # blah blah blah
    n <- nrow(mydata)
    random <- runif(n, 0, 1)
    mydata <- cbind(mydata, random)
    cross1 <- subset (mydata, as.numeric(mydata[, ncol(mydata)]) > 0.50)
    cross2 <-
      subset (mydata, as.numeric(mydata[, ncol(mydata)]) <= 0.50)
    cross1 <- cross1[-ncol(cross1)]
    cross2 <- cross2[-ncol(cross2)]
    mydata <- mydata[-ncol(mydata)]
    
    LL.btn <- c()
    split.btn <- c()
    cov.btn.names <- c()
    cov.btn.cols <- c()
    cov.type <- c()
    
    #Baseline model to compare subgroup fits to
    modelnew <- mxAddNewModelData(model, cross1, name = "BASE MODEL C1")
    LL.overall <- safeRunAndEvaluate(modelnew)
    suppressWarnings(if (is.na(LL.overall))
      return(NULL))
    n.comp <- 0
    
    #browser()
    
    if (control$report.level > 2) {
      report("Phase I - Select within variables", 1)
    }
    
    #
    
    # Step I - use cross validation fold 1 to evaluate all splits and
    #   select best split
    #
    
    # iterate over all variables
    for (cur_col in meta$covariate.ids) {
      LL.baseline <- LL.overall
      missingModel <- missingDataModel(modelnew, cross1, cur_col)
      if (!is.null(missingModel)) {
        LL.baseline <- safeRunAndEvaluate(missingModel)
      }
      if (control$report.level > 10) {
        report(paste("Estimating baseline likelihood: ", LL.baseline), 2)
      }
      
      
      # if (control$verbose)
      #    message("Testing ",c,"/",ncol(cross1), " (",colnames(cross1)[cur_col],")" )
      
      if (control$report.level >= 1 || control$verbose) {
        ui_message(paste(
          "Testing predictor",
          colnames(cross1)[cur_col],
          " (#",
          cur_col,
          "/",
          ncol(cross1),
          ")"
        ),
        2)
      }
      
      LL.within <- base::c()
      within.split <- base::c()
      
      #case for factored covariates##############################
      if (is.factor(cross1[, cur_col])) {
        #unordered factors#####################################
        if (!is.ordered(cross1[, cur_col])) {
          var.type = 1
          v <- as.numeric(cross1[, cur_col])
          val.sets <- sort(union(v, v))
          #cat("Length", length(val.sets),":",paste(v),"\n")
          if (length(val.sets) > 1) {
            #create binaries for comparison of all combinations
            result <-
              recodeAllSubsets(cross1[, cur_col], colnames(cross1)[cur_col], use.levels =
                                 T)
            test1 <- rep(0, length(cross1[, cur_col]))#base::c()
            test2 <- rep(NA, length(cross1[, cur_col]))
            
            for (j in 1:ncol(result$columns)) {
              #cat("RUN",j,"\n")
              test1 <- rep(0, length(cross1[, cur_col]))
              for (i in 1:length(cross1[, cur_col])) {
                if (isTRUE(result$columns[i, j])) {
                  test1[i] <- 1
                }
                else {
                  test1[i] <- 0
                }
              }
              test1 <- as.factor(test1)
              test2 <- data.frame(test2, test1)
            }
            test2 <- test2[, -1]
            for (i in 1:(result$num_sets)) {
              LL.temp <- base::c()
              #subset data for chosen value and store LL
              if (result$num_sets == 1) {
                vec = test2
              }
              else {
                vec = test2[[i]]
              }
              subset1 <- subset (cross1, as.numeric(vec) == 2)
              subset2 <- subset (cross1, as.numeric(vec) == 1)
              
              # refit baseline model with focus parameters @TAGX
              # for each new potential split
              if (!is.null(constraints) &
                  (!is.null(constraints$focus.parameters))) {
                LL.baseline <- fitSubmodels(model,
                                            subset1,
                                            subset2,
                                            control,
                                            invariance = constraints$focus.parameters)
                if (control$report.level > 10) {
                  report(
                    paste(
                      "Reestimating baseline likelihood with focus parameters: ",
                      LL.baseline
                    ),
                    2
                  )
                }
              }
              
              #catch LLR for each comparison, only if valid
              LL.return <-
                fitSubmodels(model, subset1, subset2, control, invariance = NULL)
              
              if (nrow(subset1) + nrow(subset2) != nrow(cross1)) {
                message("INCONSISTENCY ERROR DETECTED")
                
                LL.return <- NA
              }
              
              if (!is.na(LL.return)) {
                LL.within <- cbind(LL.within, (LL.baseline - LL.return))
                within.split <- cbind(within.split, i)
              }
            }
          }
        }
        
        
        #ordered factors#########################################
        if (is.ordered(cross1[, cur_col])) {
          var.type = 2
          v <- as.numeric(as.character(cross1[, cur_col]))
          val.sets <- sort(union(v, v))
          if (length(val.sets) > 1) {
            for (i in 2:(length(val.sets))) {
              LL.temp <- base::c()
              #subset data for chosen value and store LL
              cond1 <-
                as.numeric(as.character(cross1[, cur_col])) > (val.sets[i] + val.sets[(i -
                                                                                         1)]) / 2
              cond2 <-
                as.numeric(as.character(cross1[, cur_col])) < (val.sets[i] + val.sets[(i -
                                                                                         1)]) / 2
              subset1 <- subset (cross1, cond1)
              subset2 <- subset (cross1, cond2)
              
              # refit baseline model with focus parameters @TAGX
              if (!is.null(constraints) &
                  (!is.null(constraints$focus.parameters))) {
                LL.baseline <- fitSubmodels(model,
                                            subset1,
                                            subset2,
                                            control,
                                            invariance = constraints$focus.parameters)
                if (control$report.level > 10) {
                  report(
                    paste(
                      "Reestimating baseline likelihood with focus parameters: ",
                      LL.baseline
                    ),
                    2
                  )
                }
              }
              
              #catch LLR for each comparison
              LL.return <-
                fitSubmodels(model, subset1, subset2, control, invariance = NULL)
              if (!is.na(LL.return)) {
                LL.within <- cbind(LL.within, (LL.baseline - LL.return))
                within.split <-
                  cbind(within.split, (val.sets[i] + val.sets[(i - 1)]) / 2)
              }
            }
          }
        }
      }
      
      #numeric (continuous) covariates################################
      if (is.numeric(cross1[, cur_col])) {
        var.type = 2
        v <- as.numeric(cross1[, cur_col])
        val.sets <- sort(union(v, v))
        #if(length(val.sets) < 30|!isTRUE(control$shortcut)){
        if (length(val.sets) > 1) {
          for (i in 2:(length(val.sets))) {
            LL.temp <- base::c()
            #subset data for chosen value and store LL
            cond1 <-
              as.numeric(cross1[, cur_col]) > (val.sets[i] + val.sets[(i - 1)]) / 2
            cond2 <-
              as.numeric(cross1[, cur_col]) < (val.sets[i] + val.sets[(i - 1)]) / 2
            subset1 <- subset (cross1, cond1)
            subset2 <- subset (cross1, cond2)
            
            # refit baseline model with focus parameters @TAGX
            if (!is.null(constraints) &
                (!is.null(constraints$focus.parameters))) {
              LL.baseline <- fitSubmodels(model,
                                          subset1,
                                          subset2,
                                          control,
                                          invariance = constraints$focus.parameters)
              if (control$report.level > 10) {
                report(
                  paste(
                    "Reestimating baseline likelihood with focus parameters: ",
                    LL.baseline
                  ),
                  2
                )
              }
            }
            
            #catch LLR for each comparison
            LL.return <-
              fitSubmodels(model, subset1, subset2, control, invariance = NULL)
            if (!is.na(LL.return)) {
              LL.within <- cbind(LL.within, (LL.baseline - LL.return))
              within.split <-
                cbind(within.split, (val.sets[i] + val.sets[(i - 1)]) / 2)
            } else {
              if (control$verbose)
                ui_fail("LL was NA when fitting submodels!")
              if (control$report.level > 2) {
                report(paste("Could not estimate split at value ", val.sets[i]),
                       2)
              }
            }
          }
        }
        #}
      }
      
      if (control$report.level > 10) {
        if (!is.null(LL.within)) {
          report(paste("Within Likelihoods ", paste(round(
            LL.within, 2
          ), collapse = " ")), 2)
        }
        else{
          message("Within LLs NULL")
        }
      }
      
      if (control$report.level > 0) {
        if (is.null(LL.within)) {
          report("No valid estimate found for any split value during the first round.",
                 2)
        }
      }
      
      max.LL.within <- base::c()
      max.within.split <- base::c()
      
      #store the LL, split value and variable number for each cov that makes a possible split
      if (!is.null(LL.within)) {
        max.LL.within <- LL.within[1]
        max.within.split <- within.split[1]
        max.within.cov <- cur_col
        
        if (length(LL.within) > 1) {
          for (i in 2:length(LL.within)) {
            if (!is.na(LL.within[i]) | !is.null(LL.within[i])) {
              if (max.LL.within < LL.within[i]) {
                max.LL.within <- LL.within[i]
                max.within.split <- within.split[i]
              }
            }
          }
          
        }
        
        #max.LL.within
        #max.within.split
        
        LL.btn <- cbind(LL.btn, max.LL.within)
        split.btn <- cbind(split.btn, max.within.split)
        #cov.btn.names <- cbind(cov.btn.names, colnames(mydata[cur_col]))
        cov.btn.cols <- cbind(cov.btn.cols, max.within.cov)
        cov.type <- cbind(cov.type, var.type)
        
        if (control$report.level >= 3) {
          report(paste(
            "Best split at ",
            max.within.split,
            " with LL",
            max.LL.within
          ),
          2)
        }
        #cat("COV ",c," maxLL", max.LL.within," Within:", max.within.cov,":",max.within.split,"\n")
      }
      
    }
    
    #
    # Phase II - select between variables using their best split
    #     use cross validation fold 2 for evaluation
    #
    
    
    if (control$report.level > 2) {
      report("Phase II - Select between variables", 1)
    }
    
    #Baseline model to compare subgroup fits to
    modelnew <- mxAddNewModelData(model, cross2, name = "BASE MODEL C2")
    LL.overall <- safeRunAndEvaluate(modelnew)
    suppressWarnings(if (is.na(LL.overall)) {
      warning("Baseline likelihood is N/A; Aborting!")
      
      return(NULL)
    })
    n.comp <- ncol(LL.btn)
    
    LL.max <- NULL
    
    if (!is.null(LL.btn)) {
      for (cur_col in 1:length(LL.btn)) {
        LL.temp <- base::c()
        num.rows <- nrow(cross2)
        LL.baseline <- LL.overall
        missingModel <-
          missingDataModel(modelnew, cross2, cov.btn.cols[cur_col])
        if (!is.null(missingModel)) {
          LL.baseline <- safeRunAndEvaluate(missingModel)
          num.rows <- dim(missingModel@data@observed)[1]
        }
        
        if (cov.type[cur_col] == 1) {
          if (!is.ordered(cross2[, cov.btn.cols[cur_col]])) {
            result <-
              recodeAllSubsets(cross2[, (cov.btn.cols[cur_col])], colnames(cross2)[(cov.btn.cols[cur_col])], use.levels =
                                 T)
            test1 <- base::c()
            clen <- dim(cross2)[1]
            test2 <- rep(NA, clen)
            for (j in 1:result$num_sets) {
              test1 <- rep(0, clen)
              for (i in 1:clen) {
                if (isTRUE(result$columns[i, j])) {
                  test1[i] <- 1
                }
                else {
                  test1[i] <- 0
                }
              }
              test1 <- as.factor(test1)
              test2 <- data.frame(test2, test1)
            }
            test2 <- test2[, -1]
            #browser()
            
            
            if (result$num_sets == 1) {
              vec <- test2
              #   subset1 <- subset (cross2, as.numeric(test2) == 2)
              #  subset2 <- subset (cross2, as.numeric(test2) == 1)
            }
            else {
              vec <- test2[[split.btn[cur_col]]]
            }
            
            # if (length(unique(vec))==1) browser();
            
            subset1 <- subset (cross2, as.numeric(vec) == 2)
            subset2 <- subset (cross2, as.numeric(vec) == 1)
            
          }
        }
        else if (cov.type[cur_col] == 2) {
          cond1 <-
            as.numeric(as.character(cross2[, cov.btn.cols[cur_col]])) > split.btn[cur_col]
          cond2 <-
            as.numeric(as.character(cross2[, cov.btn.cols[cur_col]])) <= split.btn[cur_col]
          subset1 <- subset(cross2, cond1)
          subset2 <- subset(cross2, cond2)
        }
        
        # refit baseline model with focus parameters @TAGX
        if (!is.null(constraints) &
            (!is.null(constraints$focus.parameters))) {
          result <- fitSubmodels(
            model,
            subset1,
            subset2,
            control,
            invariance = constraints$focus.parameters,
            return.models = TRUE
          )
          if (control$report.level > 10) {
            report(
              paste(
                "Reestimating baseline likelihood with focus parameters: ",
                LL.baseline
              ),
              2
            )
          }
          if (!all(is.na(result))) {
            LL.baseline <- result$LL.sum
            num.rows <-
              nrow(subset1) + nrow(subset2) # THIS DEFINITELY IS A HACK. SHOULD BE
            # DEFINED BY THE RETURNED MODEL
          } else {
            #warning("LL.sum is NA after reestimation with focus parameter")
            ui_warn(
              "Could not estimate constrained/focus model for variable ",
              colnames(cross2)[cur_col],
              ". Possibly choosing a suboptimal split."
            )
            LL.baseline <- NA
            num.rows <- nrow(subset1) + nrow(subset2)
          }
        }
        
        # evaluate split
        LL.cur <-
          LL.baseline - fitSubmodels(model, subset1, subset2, control, invariance =
                                       NULL)
        # browser()
        #if(is.null(LL.cur)) LL.cur <- NA
        
        # cross2 == subset of data for Phase II
        
        if (nrow(subset1) + nrow(subset2) != num.rows) {
          #browser()
          ui_fail(
            paste(
              "SERIOUS INCONSISTENCY ERROR. Numbers of rows do not match. Type="
            ),
            cov.type[cur_col],
            " Nums=",
            nrow(subset1),
            "+",
            nrow(subset2),
            " != ",
            num.rows
          )
          LL.cur <- NA
        }
        
        if (control$verbose)
          ui_message(paste("Testing", cur_col, ":", names(mydata[cov.btn.cols[cur_col]]), " ", LL.cur, "\n"))
        
        
        if (cur_col == 1) {
          LL.max <- LL.cur
          LL.btn <- LL.cur
          split.max <- split.btn[1]
          name.max <- names(mydata[cov.btn.cols[1]])
          col.max <- cov.btn.cols[1]
          type.max <- cov.type[1]
        }
        else {
          LL.btn <- cbind(LL.btn, LL.cur)
          if (!is.na(LL.cur) & !is.null(LL.cur)) {
            if (is.na(LL.max) || is.na(LL.cur) || LL.max < LL.cur) {
              LL.max <- LL.cur
              split.max <- split.btn[cur_col]
              name.max <- names(mydata[cov.btn.cols[cur_col]])
              col.max <- cov.btn.cols[cur_col]
              type.max <- cov.type[cur_col]
            }
          }
        }
        
        if (control$report.level > 2) {
          report(paste("Name", names(mydata[cov.btn.cols[cur_col]]), "LL:", LL.cur), 2)
        }
        
      }
      
    }
    else {
      return(NULL)
    }
    ##
    # III
    #
    # Optional third step in Fair split - test all splits on best selected variable
    if (control$fair3Step == TRUE) {
      if (control$report.level > 2) {
        report("Phase III - Select Split in Best Variable", 1)
      }
      
      if (!is.null(constraints$focus.parameters)) {
        stop("Method 'fair3' does not yet support focus parameters in tree.constraints.")
      }
      
      # run full data on model for overall model fit
      modelnew <-
        mxAddNewModelData(model, mydata, name = "BASE MODEL FULL")
      LL.overall <- safeRunAndEvaluate(modelnew)
      suppressWarnings(if (is.na(LL.overall)) {
        warning("Overall likelihood is N/A; Aborting!")
        
        return(NULL)
      })
      n.comp <- 0
      
      # iterate over all splits in col.max
      cur_col <- col.max
      LL.baseline <- LL.overall
      missingModel <- missingDataModel(modelnew, mydata, col.max)
      if (!is.null(missingModel)) {
        LL.baseline <- safeRunAndEvaluate(missingModel)
      }
      
      if (control$verbose)
        ui_message("Testing ",
                   cur_col,
                   "/",
                   ncol(mydata),
                   " (",
                   colnames(mydata)[cur_col],
                   ")")
      
      if (control$report.level >= 1) {
        report(paste("Testing predictor", colnames(mydata)[cur_col]), 1)
      }
      
      LL.within <- base::c()
      within.split <- base::c()
      
      #case for factored covariates##############################
      if (is.factor(mydata[, cur_col])) {
        #unordered factors#####################################
        if (!is.ordered(mydata[, cur_col])) {
          var.type = 1
          v <- as.numeric(mydata[, cur_col])
          val.sets <- sort(union(v, v))
          #cat("Length", length(val.sets),":",paste(v),"\n")
          if (length(val.sets) > 1) {
            #create binaries for comparison of all combinations
            result <-
              recodeAllSubsets(mydata[, cur_col], colnames(mydata)[cur_col], use.levels =
                                 T)
            test1 <- rep(0, length(mydata[, cur_col]))#base::c()
            test2 <- rep(NA, length(mydata[, cur_col]))
            
            for (j in 1:ncol(result$columns)) {
              #cat("RUN",j,"\n")
              test1 <- rep(0, length(mydata[, cur_col]))
              for (i in 1:length(mydata[, cur_col])) {
                if (isTRUE(result$columns[i, j])) {
                  test1[i] <- 1
                }
                else {
                  test1[i] <- 0
                }
              }
              test1 <- as.factor(test1)
              test2 <- data.frame(test2, test1)
            }
            test2 <- test2[, -1]
            for (i in 1:(result$num_sets)) {
              LL.temp <- base::c()
              #subset data for chosen value and store LL
              if (result$num_sets == 1) {
                vec = test2
              }
              else {
                vec = test2[[i]]
              }
              subset1 <- subset (mydata, as.numeric(vec) == 2)
              subset2 <- subset (mydata, as.numeric(vec) == 1)
              
              #catch LLR for each comparison, only if valid
              LL.return <-
                fitSubmodels(model, subset1, subset2, control, invariance = NULL)
              
              if (nrow(subset1) + nrow(subset2) != nrow(mydata)) {
                message("INCONSISTENCY ERROR DETECTED")
                
                LL.return <- NA
              }
              
              if (!is.na(LL.return)) {
                LL.within <- cbind(LL.within, (LL.baseline - LL.return))
                within.split <- cbind(within.split, i)
              }
            }
          }
        }
        
        
        #ordered factors#########################################
        if (is.ordered(mydata[, cur_col])) {
          var.type = 2
          v <- as.numeric(as.character(mydata[, cur_col]))
          val.sets <- sort(union(v, v))
          if (length(val.sets) > 1) {
            for (i in 2:(length(val.sets))) {
              LL.temp <- base::c()
              #subset data for chosen value and store LL
              cond1 <-
                as.numeric(as.character(mydata[, cur_col])) > (val.sets[i] + val.sets[(i -
                                                                                         1)]) / 2
              cond2 <-
                as.numeric(as.character(mydata[, cur_col])) < (val.sets[i] + val.sets[(i -
                                                                                         1)]) / 2
              subset1 <- subset (mydata, cond1)
              subset2 <- subset (mydata, cond2)
              #catch LLR for each comparison
              LL.return <-
                fitSubmodels(model, subset1, subset2, control, invariance = NULL)
              if (!is.na(LL.return)) {
                LL.within <- cbind(LL.within, (LL.baseline - LL.return))
                within.split <-
                  cbind(within.split, (val.sets[i] + val.sets[(i - 1)]) / 2)
              }
            }
          }
        }
      }
      
      #numeric (continuous) covariates################################
      if (is.numeric(mydata[, cur_col])) {
        var.type = 2
        v <- as.numeric(mydata[, cur_col])
        val.sets <- sort(union(v, v))
        if (length(val.sets) > 1) {
          for (i in 2:(length(val.sets))) {
            LL.temp <- base::c()
            #subset data for chosen value and store LL
            cond1 <-
              as.numeric(mydata[, cur_col]) > (val.sets[i] + val.sets[(i - 1)]) / 2
            cond2 <-
              as.numeric(mydata[, cur_col]) < (val.sets[i] + val.sets[(i - 1)]) / 2
            subset1 <- subset (mydata, cond1)
            subset2 <- subset (mydata, cond2)
            #catch LLR for each comparison
            LL.return <-
              fitSubmodels(model, subset1, subset2, control, invariance = NULL)
            if (!is.na(LL.return)) {
              LL.within <- cbind(LL.within, (LL.baseline - LL.return))
              within.split <-
                cbind(within.split, (val.sets[i] + val.sets[(i - 1)]) / 2)
            } else {
              if (control$verbose)
                ui_fail("LL was NA when fitting submodels!")
              if (control$report.level > 2) {
                report(paste("Could not estimate split at value ", val.sets[i]),
                       2)
              }
            }
          }
        }
      }
      
      if (control$report > 10) {
        if (!is.null(LL.within)) {
          message("Within LLs ", paste(round(LL.within, 2), collapse = " "))
        }
        else{
          message("Within LLs NULL")
        }
      }
      
      if (control$report.level > 0) {
        if (is.null(LL.within)) {
          report("No valid estimate found for any split value during the first round.",
                 2)
        }
      }
      
      max.LL.within <- base::c()
      max.within.split <- base::c()
      
      #store the LL, split value and variable number for each cov that makes a possible split
      if (!is.null(LL.within)) {
        max.LL.within <- LL.within[1]
        max.within.split <- within.split[1]
        max.within.cov <- cur_col
        
        if (length(LL.within) > 1) {
          for (i in 2:length(LL.within)) {
            if (!is.na(LL.within[i]) | !is.null(LL.within[i])) {
              if (max.LL.within < LL.within[i]) {
                max.LL.within <- LL.within[i]
                max.within.split <- within.split[i]
              }
            }
          }
          
        }
        #max.LL.within
        #max.within.split
        split.max <- max.within.split
        LL.max <- max.LL.within
        
        if (control$report.level >= 3) {
          report(paste(
            "Best split at ",
            max.within.split,
            " with LL",
            max.LL.within
          ),
          2)
        }
      }
    }
    
    btn.matrix <-
      rbind(LL.btn, names(mydata[c(cov.btn.cols)]), cov.btn.cols, split.btn)
    colnames(btn.matrix) <-
      c(paste("var", seq(1, ncol(btn.matrix)), sep = ""))
    rownames(btn.matrix) <- c("LR", "variable", "column", "split val")
    
    if (control$report.level >= 2) {
      report("_____________________________", 1)
      report(paste(
        "Best predictor",
        name.max,
        "split at",
        split.max,
        "with LL",
        LL.max
      ),
      1)
      report("", 1)
    }
    
    #cat("Best ",LL.max, " ",split.max," ",name.max," ",col.max,"\n")
    return(
      list(
        LL.max = LL.max,
        split.max = split.max,
        name.max = name.max,
        col.max = col.max,
        type.max = type.max,
        n.comp = n.comp,
        btn.matrix = btn.matrix
      )
    )
  }
