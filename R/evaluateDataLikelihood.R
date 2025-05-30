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
  function(model, data, data_type = "raw", loglik="model")
  {
    if (inherits(model, "MxModel") || inherits(model, "MxRAMModel")) {

      if (loglik=="mvn") stop("Not implemented")
      
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
      
      
      
    } else if (inherits(model, "ctsemFit")) {
      
      if (loglik=="mvn") stop("Not implemented")
      
      select_ctsem_data <- intersect(colnames(model$mxobj$data$observed),
                                     colnames(data))
      
      model_ctsem <- ctsemOMX::ctFit(dat = data[select_ctsem_data],
                                     ctmodelobj = model$ctmodelobj,
                                     dataform = "wide",
                                     stationary = model$ctfitargs$stationary,
                                     fit = FALSE)
      
      model_up <- OpenMx::omxSetParameters(
        model = model_ctsem$mxobj,
        labels = names(OpenMx::omxGetParameters(model_ctsem$mxobj)),
        free = FALSE,
        values = model$mxobj$output$estimate)
      
      full_mxdata <- OpenMx::mxData(observed = model_ctsem$mxobj$data$observed,
                                    type = "raw")
      
      model_up <- setData(model = model_up, data = full_mxdata)
      
      model_up <- OpenMx::mxRun(
        model = model_up,
        silent = TRUE,
        useOptimizer = FALSE,
        suppressWarnings = TRUE)
      
      return(getLikelihood(model_up))
      
    } else if (inherits(model, "lavaan")) {

      ll <- NA
      
      if (loglik %in% c("mvn","default")) {
     
          # filter relevant variables
          model_observed_names <- model@Data@ov.names[[1]]
          data <- data[, model_observed_names]
        
          # compute likelihood based on multivariate normal density
          # and model-impled mean and covariance matrix
          implied <- lavInspect(model, "implied")
          Sigma <- implied$cov
          mu <- implied$mean
          ll<- -2*sum(lavaan_casewise_loglik_matrices(data, mu = mu, Sigma = Sigma ))
        
      } else {
     
        
        tryCatch({
          modelrun <- lavaan::lavaan(
            lavaan::parTable(model),
            data = data,
            control = list(
              optim.method = "none",
              optim.force.converged = TRUE
            )
          )
          
          # evaluate likelihood
          ll <- -2 * as.numeric(lavaan::logLik(modelrun))
        },error = function(e) {
          ui_warn("Could not evaluate lavaan model likelihood. Lavaan had the following error:\n ",e)
          
        })
        
      }
      
      return(ll) 
      
    } else {
      stop(
        "The chosen combination of parameters for semtree is not yet supported! Please use OpenMx model specification!"
      )
      
    }
  }