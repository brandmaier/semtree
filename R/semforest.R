#' Create a SEM Forest
#' 
#' Grows a SEM Forest from a template model and a dataset. This may take some
#' time.
#' 
#' 
#' @aliases semforest print.semforest plot.semforest
#' @param model A template SEM. Same as in \code{semtree}.
#' @param data A dataframe to create a forest from. Same as in \code{semtree}.
#' @param control A semforest control object to set forest parameters.
#' @param predictors An optional list of covariates. See semtree code example.
#' @param constraints An optional list of covariates. See semtree code example.
#' @param \dots Optional parameters.
#' @return A semforest object.
#' @author Andreas M. Brandmaier, John J. Prindle
#' @seealso \code{\link{semtree}}
#' @references Brandmaier, A.M., Prindle, J. J., McArdle, J. J., &
#' Lindenberger, U. (2016). Theory-guided exploration with structural equation
#' model forests. \emph{Psychological Methods}, 21(4), 566--582.
#' 
#' Brandmaier, A.M., Oertzen, T. v., McArdle, J.J., & Lindenberger, U. (2013).
#' Structural equation model trees. \emph{Psychological Methods}, 18(1),
#' 71--86.
#' @keywords tree models multivariate
#' @export
semforest <- function(model,
                      data,
                      control = NULL,
                      predictors = NULL,
                      constraints = NULL ,
                      ...)
{
  arguments <- list(...)
  
  # this is just for internal debugging use
  debugtree <- NULL
  
  
  covariates <- predictors
  
  # backwards-compatibility
  if ((!is.null(arguments)) &
      ("covariates" %in% names(arguments))) {
    if (is.null(predictors)) {
      #report(paste("Setting arguments to ",paste(arguments$covariates)),1)
      covariates <- arguments$covariates
    } else {
      stop("Cannot have predictors and covariates in SEM Tree model.")
    }
  }
  
  if ("semforest.control" %in% names(arguments)) {
    control <- arguments$semforest.control
    warning("Warning! Deprecated use of semforest.control!")
  }
  
  
  
  if ("seeds" %in% names(arguments)) {
    seeds <- arguments$seeds
  } else {
    seeds <- NULL
  }
  
  #if (inherits(model,"MxModel") || inherits(model,"MxRAMModel")) {
  #not.npsol <- (mxOption(NULL,"Default optimizer")!="NPSOL")
  #if (not.npsol) {
  #  warning("semtree recommends the use of NPSOL optimizer!")
  #}
  #}
  
  if ("with.error.handler" %in% names(arguments)) {
    with.error.handler <- arguments$with.error.handle
  } else {
    with.error.handler <- TRUE
  }
  
  result <- list()
  class(result) <- "semforest"
  
  result$param.names <- getOCParameterNames(model, data)
  if (is.null(covariates)) {
    covariates <- result$param.names$covariates
    covariate.ids <-
      sapply(covariates, function(cv) {
        which(cv == names(data))
      })
  } else {
    covariate.ids <-
      sapply(covariates, function(cv) {
        which(cv == names(data))
      })
  }
  
  # if there is no forest control object, create default, otherwise check
  # whether it is valid
  if (is.null(control)) {
    control <- semforest.control()
    ui_message("Default SEM forest settings established since no semforest.controls provided.")
  } else {
    if (checkControl(control) != TRUE) {
      stop("Unknown options in semforest.control object!")
    }
  }
  semforest.control <- control
  
  
  if (!is.na(semforest.control$semtree.control$seed)) {
    stop(
      paste(
        "Error! semtree.control object inside semforest.control has a seed.\n",
        "Instead, use seed argument of semforest() function to specify seeds for reproducible analysis!"
      )
    )
  }
  
  #
  # check whether the semtree object is valid
  if (!checkControl(semforest.control$semtree.control)) {
    ui_stop("Unknown options in semforest.control$semtree.control object!")
    
  }
  
  # pass mtry from forest to tree control
  if (!is.na(semforest.control$semtree.control$mtry)) {
    ui_stop(
      "mtry manualy set in  semforest.control$semtree.control object! Please set mtry in semforest.control object only!"
    )
  }
  semforest.control$semtree.control$mtry <- semforest.control$mtry
  
  # check whether all model variables are present in the
  # data file
  #  TODO
  
  # for score tests, model needs to run once
  if (inherits(model, "MxModel") || inherits(model, "MxRAMModel")) {
    if (!summary(model)$wasRun) {
      ui_message("Model was not run. Estimating parameters now before running the forest.")
      model <- OpenMx::mxTryHard(model)
    }
  }
  
  # create list of resampled data
  forest.data <- list()
  
  # resample data to grow forest with
  forest.data <-
    replicate(
      semforest.control$num.trees,
      forest.sample(
        data,
        mtry = semforest.control$premtry,
        covariates,
        return.oob = T,
        type = semforest.control$sampling
      ),
      simplify = F
    )
  
  
  
  
  
  # seeds
  if (is.null(seeds)) {
    seeds <- rep(NA, semforest.control$num.trees)
  } else if (length(seeds) == 1 && seeds == TRUE) {
    seeds <-
      runif(n = semforest.control$num.trees, max = .Machine$integer.max)
  }  else {
    if (length(seeds) != semforest.control$num.trees) {
      ui_stop("Number of seeds given does not match number of trees!")
    }
  }
  
  if (!is.null(debugtree)) {
    skip <- rep(TRUE, semforest.control$num.trees)
    skip[debugtree] <- FALSE
  } else {
    skip <- rep(FALSE, semforest.control$num.trees)
  }
  
  # store start time for computing time elapsed later
  start.time <- proc.time()
  
  # run the actual tree growing algorithm with a futurized version of apply
  # allows for parallel computation with different future::plan() strategies
  trees <- future.apply::future_mapply(
    FUN = semtreeApplyWrapper,
    forest.data,
    seeds,
    skip,
    MoreArgs = list(
      model = model,
      semtree.control = semforest.control$semtree.control,
      with.error.handler,
      predictors = covariates,
      constraints = constraints
    ),
    SIMPLIFY = FALSE,
    future.seed = TRUE
  )
  
  # give trees names
  for (i in 1:length(trees)) {
    if (!is.null(trees[[i]])) {
      trees[[i]]$name <- paste0("Tree #", i)
    }
  }
  
  # compute time elapsed
  elapsed <- proc.time() - start.time
  
  # postprocess to correct any erroneous trees
  trees <- lapply(X = trees, FUN = postprocess)
  
  # store all results in result object
  result$covariates <- covariates
  result$data <- data
  result$model <- model
  result$forest.data <- forest.data
  result$forest <- trees
  result$control <- semforest.control
  result$constraints <- constraints
  result$elapsed <- elapsed
  result$seeds <- seeds
  
  # tell the user that everything is fine
  ui_ok("Forest completed [took ", human_readable_time(elapsed[3]), "]")
  
  
  return(result)
}
