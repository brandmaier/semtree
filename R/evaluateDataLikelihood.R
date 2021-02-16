#'
#' Compute the Negative Two-Loglikelihood of some data given a model (either OpenMx or lavaan)
#' 
#' This helper function is used
#' in the \code{\link{semforest}} \code{\link{varimp}} and
#' \code{\link{proximity}} aggregate functions.
#' 
#' 
#' @param model A \code{\link{OpenMx}} model as used in \code{\link{semtree}}
#' and \code{\link{semforest}}.
#' @param data Data set to apply to a fitted model.
#' @param data_type Type of data ("raw", "cov", "cor")
#' @return Returns a -2LL model fit for the model
#' @author Andreas M. Brandmaier, John J. Prindle
#' @seealso \code{\link{semtree}}, \code{\link{semforest}}
#' @references Brandmaier, A.M., Oertzen, T. v., McArdle, J.J., & Lindenberger,
#' U. (2013). Structural equation model trees. \emph{Psychological Methods},
#' 18(1), 71-86.

evaluateDataLikelihood <-
  function(model, data, data_type = "raw")
  {
    if (inherits(model, "MxModel") || inherits(model, "MxRAMModel")) {
      # this is to trick the strict CRAN check
      objective <- NULL
      #
      
      #
      # Fix all Matrices A,S,F and M
      #
      model <-
        omxSetParameters(model,
                         labels = names(omxGetParameters(model)),
                         free = FALSE)
      
      #
      # run model (without any free params) on dataset
      #
      if (data_type == "raw")
      {
        data <- full_mxdata <- mxData(observed = data,
                                      type = "raw")
        
        
      } else if (data_type == "cov") {
        data <- full_mxdata <- mxData(
          observed = cov(data),
          type = "cov",
          numObs = dim(data)[1]
        )
        
      } else if (data_type == "cor") {
        data <- full_mxdata <- mxData(
          observed = cor(data),
          type = "cor",
          numObs = dim(data)[1]
        )
        
        
      } else {
        warning("data type is not supported!")
        return(NULL)
        
        
      }
      
      
      
      model <- setData(model, data)
      run <-
        OpenMx::mxRun(
          model,
          silent = TRUE,
          useOptimizer = FALSE,
          suppressWarnings = TRUE
        )
      
      result <- getLikelihood(run)
      
      
      return(result)
      
      
      
    } else if (inherits(model, "lavaan")) {
      # replace data
      #model <- mxAddNewModelData(model=model,data=data)
      
      # fix all parameters
      #model@ParTable$free <- rep(0, length(model@ParTable$free))
      
      # rerun model
      #modelrun <- try(suppressWarnings(
      #  eval(parse(text=paste(model@Options$model.type,'(lavaan::parTable(model),data=data,missing=\'',
      #                        model@Options$missing,'\')',sep="")))),silent=FALSE)
      
      modelrun <- lavaan::lavaan(
        lavaan::parTable(model),
        data = data,
        control = list(
          optim.method = "none",
          optim.force.converged = TRUE
        )
      )
      
      # evaluate likelihood
      ll <- -2 * lavaan::logLik(modelrun)

      return(ll)
      
    } else {
      stop(
        "The chosen combination of parameters for semtree is not yet supported! Please use OpenMx model specification!"
      )
      
    }
  }
