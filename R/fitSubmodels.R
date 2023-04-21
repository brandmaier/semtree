#'
#' Fit multigroup model for evaluating a candidate split
#'
#' @param model A model specification that is used as template for each of the two groups
#' @param subset1 Dataset for the first group model
#' @param subset2 Dataset for the second group model
#' @param control a \code{semtree.control} object
#' @param invariance fit models with invariant parameters if given. NULL otherwise (default).
#' @param return.models boolean. Return the fitted models
#' returns NA if fit fails
#' 
#' @export

fitSubmodels <- function(model,
                         subset1,
                         subset2,
                         control,
                         invariance = NULL,
                         return.models = FALSE)
{
  # this is to trick the strict CRAN check
  group1.objective <- NULL
  group2.objective <- NULL
  h12 <- NULL
  #
  
  
  if (!is.null(control$min.N)) {
    if (nrow(subset1) < control$min.bucket ||  nrow(subset2) < control$min.bucket) {
      if (control$verbose) {
       # message("Minimum number of cases reached!")
        
      }
      #if (control$report.level > 3) {
      #		report(paste("Minimum number of cases reached! Left tree has ",nrow(subset1), " and right tree has ",nrow(subset2)),2)
      #	}
      return(NA)
    }
  }
  #browser()
  
  if (is.null(invariance)) {
    if (inherits(model, "MxModel") || inherits(model, "MxRAMModel")) {
      ###########################################################
      ###               OPENMX USED HERE                      ###
      ###########################################################
      model1 <- mxModel(model, mxData(observed = subset1, type = "raw"))
      run1 <- safeRunAndEvaluate(model1, return.model = T)
      ##################################################
      suppressWarnings(if (any(is.na(run1)))
        return(NA))
      if (!checkModel(run1$model, control))
        return(NA)
      
      LL.sum <- run1$LL
      #other groups are compared to the chosen value and LL stored
      model2 <- mxModel(model, mxData(observed = subset2, type = "raw"))
      run2 <- safeRunAndEvaluate(model2, return.model = T)
      suppressWarnings(if (any(is.na(run2)))
        return(NA))
      if (!checkModel(run2$model, control))
        return(NA)
      
      if (!is.na(run2$LL)) {
        LL.sum <- LL.sum + run2$LL
      }
      if (return.models) {
        result <- c()
        result$LL.sum <- LL.sum
        result$model1 <- run1$model
        result$model2 <- run2$model
        return(result)
      } else {
        return(LL.sum)
      }
    } else if (control$sem.prog == 'ctsem') {
        ###########################################################
        ###               ctsemOMX USED HERE                    ###
        ###########################################################
      model1 <- try(ctsemOMX::ctFit(dat = subset1,
                                    ctmodelobj = model$ctmodelobj,
                                    dataform = "wide",
                                    retryattempts = 20), silent = TRUE)
      if (is(model1,"try-error")) {return(NA)}
      model2 <- try(ctsemOMX::ctFit(dat = subset2,
                                    ctmodelobj = model$ctmodelobj,
                                    dataform = "wide",
                                    retryattempts = 20), silent = TRUE)
      if (is(model2,"try-error")) {return(NA)}
      fit1 <- minus2logLik_from_fitted_models(model1)
      fit2 <- minus2logLik_from_fitted_models(model2)
      LL.sum <- fit1 + fit2
      } else {
      
      # check subgroups for empty columns in modeled variables...
      modelcheck <- unlist(model@Data@ov.names)
#      modelcheck <- names(model@data$observed)
      for (i in 1:length(modelcheck)) {
        if (is.nan(mean(subset1[, modelcheck[i]], na.rm = TRUE))) {
          if (control$verbose) {
            message(
              "Empty model variable found(",
              modelcheck[i],
              ") in subset1, cannot estimate model"
            )
          }
          return(NA)
        }
        if (is.nan(mean(subset2[, modelcheck[i]], na.rm = TRUE))) {
          if (control$verbose) {
            message(
              "Empty model variable found(",
              modelcheck[i],
              ") in subset2, cannot estimate model"
            )
          }
          return(NA)
        }
      }
      
      ###########################################################
      ###               lavaan USED HERE                      ###
      ###########################################################
      #if (control$verbose) {message("Evaluating Subset 1")}
      model1 <-
        try(suppressWarnings(eval(parse(
          text = paste(
            model@Options$model.type,
            '(lavaan::parTable(model),data=subset1,missing=\'',
            model@Options$missing,
            '\',do.fit=F)',
            sep = ""
          )
        ))), silent = T)
      #model1 <- try(suppressWarnings(lavaan::lavaan(lavaan::parTable(model),data=subset1,model.type=model@Options$model.type,do.fit=FALSE)),silent=TRUE)
      if (is(model1, "try-error")) {
        if (control$verbose) {
          message(paste0("Error in fitting submodel  #1", model1))
        }
        return(NA)
      }
      run1 <- safeRunAndEvaluate(model1, return.model = T)
      ##################################################
      if (is.na(run1)[1]) {
        return(NA)
      }
      if (!checkModel(run1$model, control))
        return(NA)
      
      LL.sum <- run1$LL
      #other groups are compared to the chosen value and LL stored
      #if (control$verbose) {message("Evaluating Subset 2")}
      model2 <-
        try(suppressWarnings(eval(parse(
          text = paste(
            model@Options$model.type,
            '(lavaan::parTable(model),data=subset2,missing=\'',
            model@Options$missing,
            '\',do.fit=F)',
            sep = ""
          )
        ))), silent = T)
      #model2 <- try(suppressWarnings(lavaan::lavaan(lavaan::parTable(model),data=subset2,model.type=model@Options$model.type,do.fit=FALSE)),silent=TRUE)
      if (is(model2, "try-error")) {
        if (control$verbose) {
          message(paste0("Error in fitting submodel  #2", model2))
        }
        return(NA)
      }
      run2 <- safeRunAndEvaluate(model2, return.model = T)
      if (is.na(run2)[1]) {
        return(NA)
      }
      if (!checkModel(run2$model, control))
        return(NA)
      
      if (!is.na(run2$LL)) {
        LL.sum <- LL.sum + run2$LL
      }
      if (return.models) {
        result <- c()
        result$LL.sum <- LL.sum
        result$model1 <- run1$model
        result$model2 <- run2$model
        return(result)
      } else {
        return(LL.sum)
      }
      #      browser()
    }
    
    
  }
  else {
    #
    # invariance testing works as follow:
    # build a multigroup model with all parameters unique per-group
    # but the parameters given in 'invariance'
    
    if (inherits(model, "MxModel") || inherits(model, "MxRAMModel")) {
      ###########################################################
      ###               OPENMX USED HERE                      ###
      ###########################################################
      model1 <- mxModel(model, mxData(observed = subset1, type = "raw"))
      model2 <- mxModel(model, mxData(observed = subset2, type = "raw"))
      
      model1@name = "group1"
      model2@name = "group2"
      
      
      newlabels1 <- names(omxGetParameters(model1))
      newlabels2 <- names(omxGetParameters(model2))
      
      newlabels1 <-
        stringr::str_replace_all(newlabels1, "\\[|\\]|,|\\.", "_")
      newlabels2 <-
        stringr::str_replace_all(newlabels2, "\\[|\\]|,|\\.", "_")
      
      # replace labels
      eqids <- which(newlabels1 %in% invariance)
      
      if (length(eqids) != length(invariance)) {
        warning("Not all invarianceparameters were found when evaluating sub models!")
      }
      
      # if (control$report.level > 20) {
      #   report(paste("invariance vector ids:",eqids," corresponding to labels ",invariance),1)
      # }
      
      if (length(eqids) == 0) {
        uneqids <- 1:length(newlabels1)
      } else {
        uneqids <- (1:length(newlabels1))[-eqids]
      }
      
      if (length(uneqids) == 0) {
        warning("fitSubmodels was called with all parameters equal. Possible bug warning!")
      } else {
        newlabels1[uneqids] <-
          paste("__yc90wr3jdv9234jtt__", newlabels1[uneqids], sep = "")
        newlabels2[uneqids] <-
          paste("__934jfvu35thi9g823__", newlabels2[uneqids], sep = "")
        
        model1 <-
          omxSetParameters(model1,
                           labels = names(omxGetParameters(model1)),
                           newlabels = newlabels1)
        model2 <-
          omxSetParameters(model2,
                           labels = names(omxGetParameters(model2)),
                           newlabels = newlabels2)
        
      }
      
      # modify constraints
      if (length(model@constraints)>0) {
        model1@constraints <- replaceParametersInMxConstraints(model1@constraints,
                                                         names(omxGetParameters(model)),
                                                         names(omxGetParameters(model1))
                                                         ) 
        model2@constraints <- replaceParametersInMxConstraints(model1@constraints,
                                                         names(omxGetParameters(model)),
                                                         names(omxGetParameters(model2))
        ) 
      }
      
      sharedModel <- mxModel(
        "sharedModel",
        model1,
        model2,
        mxAlgebra(group1.objective + group2.objective, name =
                    "h12"),
        mxFitFunctionAlgebra('h12')
      )
      
      sharedRun <- mxRun(sharedModel, silent = T)
      
      LL.sum <-  mxEval(h12, sharedRun)
      
      if (return.models) {
        result <- c()
        result$model1 <- sharedRun$group1
        result$model2 <- sharedRun$group2
        result$LL.sum <- LL.sum
        
        return(result)
      } else {
        return(LL.sum)
      }
      
      #return(NA)
      
    } else if (inherits(model, "lavaan")) {
      # invariance testing with lavaan
      
      joinset <- rbind(subset1, subset2)
      grp <- c(rep(1, nrow(subset1)), rep(2, nrow(subset2)))
      joinset <- cbind(joinset, grp)
      names(joinset)[length(names(joinset))] <- "yc90wr3jdv9234jtt"
      
      # TODO - change user parameter labels!
      jpart <- rbind(lavaan::partable(model), lavaan::partable(model))
      pgrp <-
        c(rep(1, nrow(lavaan::partable(model))), rep(2, nrow(lavaan::partable(model))))
      jpart$group <- pgrp
      jpart$block <- pgrp
      
      fit <-
        lavaan::sem(jpart, data = joinset, group = "yc90wr3jdv9234jtt")
      
      #
      # TODO: modify parTable and refit
      #
      ind <-
        !(jpart$label %in% invariance) &
        (jpart$label != "") & (1:length(jpart$label) <= dim(jpart)[1] / 2)
      jpart$label[ind] <-
        paste0("yc90wr3jdv9234jtt_", jpart$label[ind])
      
      modelrun <-
        try(suppressWarnings(eval(parse(
          text = paste(
            model@Options$model.type,
            '(lavaan::parTable(model),data=data,missing=\'',
            model@Options$missing,
            '\')',
            sep = ""
          )
        ))), silent = T)
      LL.sum <- -2 * lavaan::logLik(modelrun)
      #   stop("Not yet implemented!")
      
      if (return.models) {
        # result <- c()
        # result$model1 <- sharedRun$group1
        # result$model2 <- sharedRun$group2
        # result$LL.sum <- LL.sum
        stop("Not implemented for lavaan")
        #	      return(result)
      } else {
        return(LL.sum)
      }
    }
    
  } # end is.null(invariance)
} 