#' SEM Tree: Recursive Partitioning for Structural Equation Models
#'
#' Structural equation model (SEM) trees are a combination of SEM and decision
#' trees (also known as classification and regression trees or recursive
#' partitioning). SEM trees hierarchically split empirical data into
#' homogeneous groups sharing similar data patterns with respect to a SEM by
#' recursively selecting optimal predictors of these differences from a
#' potentially large set of predictors.
#'
#' Calling \code{semtree} with an \code{\link{OpenMx}} or
#' \code{\link[lavaan]{lavaan}} model creates a tree that recursively
#' partitions a dataset such that the partitions maximally differ with respect
#' to the model-predicted distributions. Each resulting subgroup (represented
#' as a leaf in the tree) is represented by a SEM with a distinct set of
#' parameter estimates.
#'
#' Predictors (yet unmodeled variables) can take on any form for the splitting
#' algorithm to function (categorical, ordered categories, continuous). Care
#' must be taken in choosing how many predictors to include in analyses because
#' as the number of categories grows for unordered categorical variables, the
#' number of multigroup comparisons increases exponentially for unordered
#' categories.
#'
#' Currently available evaluation methods for assessing partitions:
#'
#' 1. "naive" selection method compares all possible split values to one
#' another over all predictors included in the dataset.
#'
#' 2. "fair" selection uses a two step procedure for analyzing split values on
#' predictors at each node of the tree. The first phase uses half of the sample
#' to examine the model improvement for each split value on each predictor, and
#' retains the the value that presents the largest improvement for each
#' predictor. The second phase then evaluates these best split points for each
#' predictor on the second half of the sample. The best improvement for the c
#' splits tested on c predictors is selected for the node and the dataset is
#' split from this node for further testing.
#'
#' 3. "score" uses score-based test statistics. These statistics are much
#' faster than the classic SEM tree approach while having favorable
#' statistical properties.
#'
#' All other parameters controlling the tree growing process are available
#' through a separate \code{\link{semtree.control}} object.
#'
#' @aliases semtree plot.semtree print.semtree summary.semtree toLatex.semtree
#' nodeFunSemtree
#' @param model A template model specification from \code{\link{OpenMx}} using
#' the \code{\link{mxModel}} function (or a \code{\link[lavaan]{lavaan}} model
#' using the \code{\link[lavaan]{lavaan}} function with option fit=FALSE).
#' Model must be syntactically correct within the framework chosen, and
#' converge to a solution.
#' @param data Data.frame used in the model creation using
#' \code{\link{mxModel}} or \code{\link[lavaan]{lavaan}} are input here. Order
#' of modeled variables and predictors is not important when providing a
#' dataset to \code{semtree}.
#' @param control \code{\link{semtree}} model specifications from
#' \code{\link{semtree.control}} are input here. Any changes from the default
#' setting can be specified here.
#' @param constraints A \code{\link{semtree.constraints}} object setting model
#' parameters as constrained from the beginning of the \code{semtree}
#' computation. This includes options to globally or locally set equality
#' constraints and to specify focus parameters (i.e., parameter subsets that
#' exclusively go into the function evaluating splits). Also, options for
#' measurement invariance testing in trees are included.
#' @param predictors A vector of variable names matching variable names in
#' dataset. If NULL (default) all variables that are in dataset and not part of
#' the model are potential predictors. Optional function input to select a
#' subset of the unmodeled variables to use as predictors in the \code{semtree}
#' function.
#' @param \dots Optional arguments passed to the tree growing function.
#' @return A \code{semtree} object is created which can be examined with
#' \code{summary}, \code{plot}, and \code{print}.
#' @author Andreas M. Brandmaier, John J. Prindle, Manuel Arnold
#' @seealso \code{\link{semtree.control}}, \code{\link{summary.semtree}},
#' \code{\link{parameters}}, \code{\link{se}}, \code{\link{prune.semtree}},
#' \code{\link{subtree}}, \code{\link[OpenMx]{OpenMx}},
#' \code{\link[lavaan]{lavaan}}
#' @references
#' Brandmaier, A.M., Oertzen, T. v., McArdle, J.J., & Lindenberger, U. (2013). Structural equation model trees. \emph{Psychological Methods}, 18(1), 71-86.
#' @references
#' Arnold, M., Voelkle, M. C., & Brandmaier, A. M. (2021). Score-guided structural equation model trees. \emph{Frontiers in Psychology}, 11, Article 564403. https://doi.org/10.3389/fpsyg.2020.564403
#'
#' @keywords tree models multivariate
#'
#' @export
semtree <- function(model, data = NULL, control = NULL, constraints = NULL,
                    predictors = NULL, ...) {
  
  # some checks on the data
  if (!is.null(data)) {
    if (!is.data.frame(data)) {
      stop("Error with 'data' argument: semtree currently only supports data frames.")
    }
  }
  
  # TODO: change this throughout
  dataset <- data

  # obtain dots arguments and test for deprecated use of arguments
  arguments <- list(...)
  if ("global.constraints" %in% names(arguments)) {
    stop("Deprecated use of argument 'global.constraints'. Please use constraints object")
  }
  if ("invariance" %in% names(arguments)) {
    stop("Deprecated use of argument 'invariance'. Please use constraints object with property 'local.invariance'")
  }

  if (is.null(constraints)) {
    constraints <- semtree.constraints()
  }

  covariates <- predictors

  # backwards-compatibility
  if ((!is.null(arguments)) & ("covariates" %in% names(arguments))) {
    if (is.null(predictors)) {
      # report(paste("Setting arguments to ",paste(arguments$covariates)),1)
      covariates <- arguments$covariates
    } else {
      stop("Cannot have both arguments 'predictors' and 'covariates' in SEM Tree model.")
    }
  }



  invariance <- constraints$local.invariance
  global.constraints <- constraints$global.invariance



  # create default control object, if not specified
  if (is.null(control)) {
    control <- semtree.control()
    if (control$verbose) {
      ui_message("Default SEMtree settings established since no Controls provided.")
    }
  } else {
    if (checkControl(control) != TRUE) {
      stop("Unknown options in semtree.control object!")
    }
  }

  # here we decide between four cases depending
  # on whether min.N is given and/or min.bucket is given
  # this is a really dumb heuristic
  # please can someone replace this with something more useful
  # this based on (Bentler & Chou, 1987; see also Bollen, 1989)
  
  if (is.null(control$min.N)) {
    
    if (is.null(control$min.bucket)) {
      # both values were not specified 
      control$min.N <- max(20, 5 * npar(model))
      control$min.bucket <- max(10, control$min.N / 2)
    } else {
      # only min.bucket was given, min.N was not specified
      control$min.N <- control$min.bucket * 2
    }
  } else {
    if (is.null(control$min.bucket)) {
      # only min.N was given, min.bucket was not specified
      control$min.bucket <- max(10, control$min.N / 2)     
    } else {
      # do nothing, both values were specified
      if (control$min.bucket > control$min.N) {
        warning("Min.bucket parameter should probably be smaller than min.N!")
      }
    }
  }
  
  if (is.null(control$min.N)) {

  }

  # set min.bucket and min.N heuristically
  if (is.null(control$min.bucket)) {

  }

  if (control$method == "cv") {
    ui_stop("This method ceased to exist. Please see modern score-based tests.")
  }

  # check whether data is complete for score-tests
  # this probably should be a more fine-grained check some day
  # that tests only model variables and selected predictors
  if (control$method == "score") {
    check_complete <- all(stats::complete.cases(data))
    if (!check_complete) {
      ui_stop("If score tests are used, data must not contain N/A in either the
           predictors or model variables.")
    }
  }

  # check for correct model entry
  if (inherits(model, "MxModel") || inherits(model, "MxRAMModel")) {
    if (control$verbose) {
      message("Detected OpenMx model.")
    }
    control$sem.prog <- "OpenMx"
  } else if (inherits(model, "lavaan")) {
    # if (control$verbose) { ui_message("Detected lavaan model.") }
    control$sem.prog <- "lavaan"
  } else if ((inherits(model, "ctsemFit")) || (inherits(model, "ctsemInit"))) {
    # if (control$verbose) { ui_message("Detected ctsem model.") }
    control$sem.prog <- "ctsem"
    
    ctsemomx_omx_installed <- "ctsemOMX" %in% utils::installed.packages()[,"Package"]
    if (!ctsemomx_omx_installed) {
      stop("Please install ctsemOMX first.")
    }
    
  } else {
    ui_stop("Unknown model type selected. Use OpenMx or lavaanified lavaan models!")
  }

  # set the mtry value to default=0 if not set
  if (is.na(control$mtry)) control$mtry <- 0


  # some checks
  if (!is.null(constraints$focus.parameters)) {
    if (control$sem.prog != "OpenMx") {
      ui_stop("Focus parameters are only supported with OpenMx!")
    }

    num.match <- length(constraints$focus.parameters %in%
      OpenMx::omxGetParameters(model))
    if (num.match != length(constraints$focus.parameters)) {
      ui_stop("Error! Not all focus parameters are free parameters in the model!")
    }
  }

  # add data to model if not already done and sort covariates from model variables
  ###########################################################
  ###               OPENMX USED HERE                      ###
  ###########################################################
  if ((control$sem.prog == "OpenMx") || (control$sem.prog == "ctsem")) {
    if ((control$sem.prog == "ctsem")) {
      ## 11.08.2022: check data format. Currently, only wide format is supported.
      if (all(is.na(match(
        paste0(model$ctmodelobj$manifestNames, "_T0"),
        colnames(dataset)
      )))) {
        stop("Long format data detected. Data need to be in wide format.")
        # Check if the model unsupported components
        # to be done
      }

      model$mxobj@manifestVars <- paste0(
        model$ctmodelobj$manifestNames, "_T",
        rep(0:(model$ctmodelobj$Tpoints - 1),
          each = model$ctmodelobj$n.manifest
        )
      )
      mxmodel <- model$mxobj
    } else {
      mxmodel <- model
    }

    if (is.null(dataset)) {
      if (is.null(mxmodel@data)) {
        stop("MxModel has no data associated!")
      }
      dataset <- mxmodel@data@observed
    }

    # sanity check
    if (any(!(covariates %in% names(dataset)))) {
      stop(
        paste(
          "Some of the specified predictors are not in the dataset provided: ",
          paste(covariates[(!(covariates %in% names(dataset)))], sep = "", collapse = ",")
        )
      )
    }

    tmp <- getPredictorsOpenMx(mxmodel, dataset, covariates)
    model.ids <- tmp[[1]]
    covariate.ids <- tmp[[2]]

    # check whether character columns are given as predictors
    for (i in covariate.ids) {
      if (!is.factor(dataset[, i]) && !is.numeric(dataset[, i])) {
        # this column is neither numeric or a factor, thus cannot be handled
        # probably a vector of strings
        ui_stop("Predictor '", colnames(dataset)[i], "' is neither a factor nor numeric. This is likely causing trouble. Please remove or specify as factor or ordered.")
      }
    }

    # check whether numeric covariates have more than 9 observed values
    # if score-tests are used, otherwise score statistics can become
    # unstable
    if (control$method == "score") {
      for (i in covariate.ids) {
        if (!is.factor(dataset[, i]) && is.numeric(dataset[, i])) {
          # this column is numeric, should have more than 9 unique values!
          check_9levels <- length(unique(dataset[, i])) > 9
          if (!check_9levels) {
            ui_warn("Predictor '", colnames(dataset)[i], "' has 9 or fewer unique values. Consider coding as ordinal to avoid instability with score-based tests.")
          }
        }
      }
    }

    # 15.08.2022: all OpenMx models are estimated here if not already estimated
    ## ctsem are already estimated once
    if (control$sem.prog == "OpenMx" && !summary(model)$wasRun) {
      ui_message("Model was not run. Estimating parameters now.")
      suppressMessages(model <- OpenMx::mxTryHard(model = model, paste = FALSE, silent = TRUE))
    }


    # Prepare objects for fast score calculation
    ## Only for linear models (semtree$linear == TRUE) or for models with definition variables
    # Note: model must be run - this is assured by previous code block that performs mxTryHard()
    if (control$method == "score" & control$sem.prog == "OpenMx") {
      control <- c(
        control,
        list(scores_info = OpenMx_scores_input(
          x = model,
          control = control
        ))
      )
    }
  }

  ###########################################################
  ###               lavaan USED HERE                      ###
  ###########################################################
  if (control$sem.prog == "lavaan") {
    if (is.null(dataset)) {
      ui_stop("Must include data for analysis!")
    }

    tmp <- getPredictorsLavaan(model, dataset, covariates)
    model.ids <- tmp[[1]]
    covariate.ids <- tmp[[2]]
  }

  meta <- list()
  meta$model.ids <- model.ids
  meta$covariate.ids <- covariate.ids

  # init unique node counter
  # 	assign("global.node.id",1,envir = getSemtreeNamespace())
  # TODO: is there a better way to assign ids?
  setGlobal("global.node.id", 1)

  # create default constraints if none specified for invariance testing of nested models
  if (is.null(invariance)) {
    invariance <- NULL
  } else {
    if (control$method != "naive") {
      ui_message("Invariance is only implemented for naive variable selection.")
      return(NULL)
    }
    if (is.na(control$alpha.invariance)) {
      ui_message("No Invariance alpha selected. alpha.invariance set to:", control$alpha)
      control$alpha.invariance <- control$alpha
    }

    if (is(invariance, "character")) {
      invariance <- list(invariance)
    } else {
      if (!is(invariance, "list")) {
        ui_stop("Invariance must contain an array of parameter names or a list of such arrays.")
      }
    }
  }

  # heuristic checks whether variables are correctly coded
  # to avoid problems in the computation of test statistics
  for (cid in covariate.ids) {
    column <- dataset[, cid]
    if (is.numeric(column)) {
      if (length(unique(column)) <= 10) {
        ui_warn("Variable ", names(dataset)[cid], " is numeric but has only few unique values. Consider recoding as ordered factor.")
      }
    }
  }

  # check for no missing data in covariates if score statistics are used
  if (control$method == "score") {
    for (cid in covariate.ids) {
      column <- dataset[, cid]
      if (sum(is.na(column)) > 0) {
        ui_stop("Variable ", names(dataset)[cid], " has missing values. Computation of score statistic not possible.")
        return(NULL)
      }
    }
  }



  # correct method selection check
  method.int <- pmatch(control$method, c("cv", "naive", "fair", "fair3", "score"))
  if (is.na(method.int)) {
    ui_stop("Unknown method in control object! Try either 'naive', 'fair', 'fair3', 'score', or 'cv'.")
  }

  # if this is still null, no data was given
  if (is.null(dataset)) {
    ui_stop("No data were provided!")
  }

  # sanity checks, duplicated col names?
  if (any(duplicated(names(dataset)))) {
    ui_stop("Dataset contains duplicated columns names!")
  }

  # set a seed for user to repeat results if no seed provided
  if (!is.null(control$seed) & !is.na(control$seed)) {
    set.seed(control$seed)
  }
  ###########################################################
  ###               OPENMX USED HERE                      ###
  ###########################################################
  # global constraints - estimate once and then regarded fixed in the tree
  if (!is.null(global.constraints)) {
    if (control$sem.prog != "OpenMx") {
      ui_stop("Global constraints are not yet supported!")
    }

    run.global <- OpenMx::mxRun(model, silent = TRUE, useOptimizer = TRUE, suppressWarnings = TRUE)
    labels <- names(OpenMx::omxGetParameters(model))
    eqids <- which(labels %in% global.constraints)
    neqids <- which(!labels %in% global.constraints)
    values <- OpenMx::omxGetParameters(run.global)[eqids]
    model <- OpenMx::omxSetParameters(model,
      labels = global.constraints, free = FALSE, values = values
    )
    # FIX THIS LINE HERE

    # Read Global Constraints and New model Parameters Here.
    ui_message("Global Constraints:\n", paste(global.constraints, collapse = " "))
    ui_message("Freely Estimated Parameters:\n", paste(names(OpenMx::omxGetParameters(model)), collapse = " "))
  }


  # grow tree
  if (control$sem.prog == "OpenMx") {
    if (control$verbose) {
      message("OpenMx model estimation selected!")
    }
  } else if (control$sem.prog == "lavaan") {
    if (control$verbose) {
      message("lavaan model estimation selected!")
    }
  } else if (control$sem.prog == "ctsem") {
    if (control$verbose) {
      message("ctsem model estimation selected!")
    }
  } else {
    stop("Unknown model type. Use OpenMx or lavaans models only!")
  }



  # save time before starting the actual tree growing
  start.time <- proc.time()

  # start the recursive growTree() function to do the
  # actual heavy lifting
  tree <- growTree(
    model = model, mydata = dataset, control = control,
    invariance = invariance, meta = meta,
    constraints = constraints, ...
  )


  # determine time elapsed
  elapsed <- proc.time() - start.time


  # save various information in the result object and
  # assign it class 'semtree'
  tree$elapsed <- elapsed
  tree$control <- control
  tree$constraints <- constraints
  tree$version <- tryCatch(sessionInfo()$otherPkgs$semtree$Version)
  class(tree) <- "semtree"

  # tell the user that everything is OK
  ui_ok(
    "Tree construction finished [took ",
    human_readable_time(elapsed[3]), "]."
  )

  return(tree)
}
