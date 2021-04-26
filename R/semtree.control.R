#' SEM Tree Control Object
#' 
#' A \code{semtree.control} object contains parameters that determine the tree
#' growing process. These parameters include choices of different split
#' candidate selection procedures and hyperparameters of those. Calling the
#' constructor without parameters creates a default control object. A number of
#' tree growing methods are included in with this package: 1. "naive" splitting
#' takes the best split value of all possible splits on each covariate. 2.
#' "fair" selection is so called because it tests all splits on half of the
#' data, then tests the best split value for each covariate on the other half
#' of the data. The equal footing of each covariate in this two phase test
#' removes bias from testing variables with many possible splits compared to
#' those with few. 3. "fair3" does the phases described above, with an
#' additional step of retesting all of the split values on the best covariate
#' found in the second phase. Variations in the sample from subsetting are
#' removed and bias in split selection further reduced. 4. "crossvalidation"
#' partitions the data for maximizing splits on each variable, then comparing
#' maximum splits across each variable on the rest of the data.
#' 
#' 
#' @aliases semtree.control print.semtree.control
#' @param method Default: "naive". One out of
#' \code{c("fair","fair3","naive","cv")} for either an unbiased two-step
#' selection algorithm, three-step fair algorithm, a naive take-the-best, or a
#' cross-validation scheme.
#' @param min.N Default: 10. Minimum sample size per a node, used to determine
#' whether to continue splitting a tree or establish a terminal node.
#' @param max.depth Default: NA. Maximum levels per a branch. Parameter for
#' limiting tree growth.
#' @param alpha Default: 0.05. Significance level for splitting at a given
#' node.
#' @param alpha.invariance Default: NA. Significance level for invariance
#' tests. If NA, the value of alpha is used.
#' @param folds Default: 5. Defines the number of folds for the \code{"cv"}
#' method.
#' @param exclude.heywood Default: TRUE. Reports whether there is an
#' identification problem in the covariance structure of an SEM tested.
#' @param progress.bar Default: NA. Option to disable the progress bar for tree
#' growth.
#' @param verbose Default: FALSE. Option to turn on or off \emph{all} model
#' messages during tree growth.
#' @param bonferroni Default: FALSE. Correct for multiple tests with Bonferroni
#' type correction.
#' @param seed Default: NA. Set a random number seed for repeating random fold
#' generation in tree analysis.
#' @param custom.stopping.rule Default: NA. Otherwise, this can be a boolean
#' function with a custom stopping rule for tree growing.
#' @param exclude.code Default: NA. NPSOL error code for exclusion from model
#' fit evaluations when finding best split. Default: Models with errors during
#' fitting are retained.
#' @param mtry Default: NA. Number of sample columns to use in SEMforest
#' analysis.
#' @param report.level Default: 0. Values up to 99 can be used to increase the
#' number of onscreen reports for semtree analysis.
#' @param use.all Treatment of missing variables. By default, missing values
#' stay in a decision node. If TRUE, cases are distributed according to a
#' maximum likelihood principle to the child nodes.
#' @param score.tests A list of score-based test statistics from the
#' strucchange package to be used for different variable types.
#' @param information.matrix A function to extract the covariance matrix for
#' the coefficients of the fitted model.
#' @param scaled_scores If TRUE (default), a scaled cumulative score process is
#' used for identifying a cutpoint.
#' @param linear If TRUE (default), the structural equation model is assumed to
#' be linear without any nonlinear parameter constraints. The runtime is much
#' smaller for linear MxRAM-type models than for models with nonlinear
#' constraints on the parameters.
#' @param min.bucket Minimum bucket size to continue splitting
#' @param naive.bonferroni.type Default: 0. When set to zero, bonferroni
#' correction for the naive test counts the number of dichotomous tests. When
#' set to one, bonferroni correction counts the number of variables tested.
#' @param missing Missing value treatment. Default is ignore
#' @param use.maxlm Use MaxLm statistic
#' @param strucchange.from Strucchange argument. See their package
#' documentation.
#' @param strucchange.to Strucchange argument. See their package documentation.
#' @param strucchange.nrep Strucchange argument. See their package
#' documentation.
#' @return A control object containing a list of the above parameters.
#' @author Andreas M. Brandmaier, John J. Prindle, Manuel Arnold
#' @seealso \code{\link{semtree}}
#' @references Brandmaier, A.M., Oertzen, T. v., McArdle, J.J., & Lindenberger,
#' U. (2013). Structural equation model trees. \emph{Psychological Methods},
#' 18(1), 71-86.
#' @examples
#' 
#' 
#' 	# create a control object with an alpha level of 1%
#' 	my.control <- semtree.control(alpha=0.01)
#' 
#' 	# set the minimum number of cases per node to ten
#' 	my.control$min.N <- 10
#' 	
#' 	# print contents of the control object
#' 	print(my.control)
#' 
#' 
#' @export
semtree.control <-
  function(method = "naive",
           min.N = 20,
           max.depth = NA,
           alpha = .05,
           alpha.invariance = NA,
           folds = 5,
           exclude.heywood = TRUE,
           progress.bar = TRUE,
           verbose = FALSE,
           bonferroni = FALSE,
           use.all = FALSE,
           seed = NA,
           custom.stopping.rule = NA,
           mtry = NA,
           report.level = 0,
           exclude.code = NA,
           score.tests = list(nominal = 'LMuo',
                              ordinal = 'maxLMo',
                              metric = 'maxLM'),
           information.matrix = "info",
           scaled_scores = TRUE,
           linear = TRUE,
           min.bucket = 10,
           naive.bonferroni.type = 0,
           missing = 'ignore',
           use.maxlm = FALSE,
           strucchange.from = 0.15,
           strucchange.to = NULL,
           strucchange.nrep = 50000)
  {
    options <- list()
    # verbose output during generation of SEMTree
    options$verbose <- verbose
    # score tests for each scale type
    options$score.tests <- lapply(X = score.tests, FUN = tolower)
    if (options$score.tests$metric == "maxlm") {
      options$score.tests$metric <- "suplm"
    }
    # information matrix used to decorrelate scores
    options$information.matrix <- information.matrix
    # Scale scores for testing continuous covariates
    options$scaled_scores <- scaled_scores
    # For OpenMx models: Is the model linear?
    ### Global Invariant parameters are currently not working with the speed up
    options$linear <- linear
    # number of cross validation folds
    options$num.folds <- folds
    # individual CV folds for data under missingess (should be no option in the long run)
    #options$individual.cv.folds <- TRUE
    # exclude heywood cases from further evaluation
    options$exclude.heywood <- exclude.heywood
    # minimum number of cases for SEM evaluation
    options$min.N <- min.N
    # minimum number of cases in leaf
    options$min.bucket <- min.bucket
    # method
    options$method <- method
    # maximal depth of the tree , set to NA for unrestricted trees
    options$max.depth <- max.depth
    # test invariance of strong restrictions
    #options$test.invariance <- test.invariance
    # alpha level
    options$alpha <- alpha
    # invariance alpha
    options$alpha.invariance <- alpha.invariance
    # progress bar
    options$progress.bar <- progress.bar
    # validation function for models
    options$validator.function <- NULL
    # bonferroni
    options$bonferroni <- bonferroni
    #Use all cases with NA on split vars
    options$use.all <- use.all
    # model fit exclusion (6 = Status RED)
    options$exclude.code <- exclude.code
    
    # Seed value
    options$seed <- seed
    # mtry (for forests only)
    options$mtry <- mtry
    # custom stopping rule
    options$custom.stopping.rule <- custom.stopping.rule
    # report level (similar to verbose but prettier)
    options$report.level <- report.level
    # type of counting the number of tests (0=all splits, 1=# of variables)
    options$naive.bonferroni.type <- naive.bonferroni.type
    # missing data treatment
    options$missing <- missing
    # max LM stat
    options$use.maxlm <- use.maxlm
    # from (for strucchange)
    options$strucchange.from <- strucchange.from
    # to (for strucchange)
    if (is.null(strucchange.to)) {
      strucchange.to <- 1 - strucchange.from
    }
    options$strucchange.to <- strucchange.to
    # nrep (for strucchange)
    options$strucchange.nrep <- strucchange.nrep
    
    
    class(options) <- "semtree.control"
    
    return(options)
  }
