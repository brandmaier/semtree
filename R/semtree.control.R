#' SEM Tree Control Object
#' 
#' A \code{semtree_control} object contains parameters that determine the tree
#' growing process. These parameters include choices of different split
#' candidate selection procedures and hyperparameters of those. Calling the
#' constructor without parameters creates a default control object. A number of
#' tree growing methods are included in with this package: 1. 'naive' splitting
#' takes the best split value of all possible splits on each covariate. 2.
#' 'fair' selection is so called because it tests all splits on half of the
#' data, then tests the best split value for each covariate on the other half
#' of the data. The equal footing of each covariate in this two phase test
#' removes bias from testing variables with many possible splits compared to
#' those with few. 3. "fair3" does the phases described above, with an
#' additional step of retesting all of the split values on the best covariate
#' found in the second phase. Variations in the sample from subsetting are
#' removed and bias in split selection further reduced. 4. 'score'
#' implements modern score-based statistics.
#' 
#' 
#' @aliases semtree.control print.semtree.control semtree_control
#' @param method Default: 'naive'. One out of
#' \code{c("score","fair","naive")} for either an unbiased two-step
#' selection algorithm,  a naive take-the-best, or a
#' score-based testing scheme.
#' @param min.N Default: 10. Minimum sample size per a node, used to determine
#' whether to continue splitting a tree or establish a terminal node.
#' @param max.depth Default: NA. Maximum levels per a branch. Parameter for
#' limiting tree growth.
#' @param alpha Default: 0.05. Significance level for splitting at a given
#' node.
#' @param alpha.invariance Default: NA. Significance level for invariance
#' tests. If NA, the value of alpha is used.
#' @param exclude.heywood Default: TRUE. Reports whether there is an
#' identification problem in the covariance structure of an SEM tested.
#' @param progress.bar Default: NA. Option to disable the progress bar for tree
#' growth.
#' @param verbose Default: FALSE. Option to turn on or off \emph{all} model
#' messages during tree growth.
#' @param bonferroni Default: FALSE. Correct for multiple tests with Bonferroni
#' type correction. p-values are adjusted for the number of variables tested.
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
#' @param linear If TRUE (default), the structural equation model is assumed to
#' not contain any nonlinear parameter constraints and scores are computed
#' analytically, resulting in a shorter runtime. Only relevant for models fitted
#' with OpenMx.
#' @param min.bucket Minimum bucket size. This is the minimum size any node
#' must have, such that a given split is considered valid. Minimum bucket size
#' is a lower bound to the sample size in the terminal nodes of a tree.
#' @param missing Missing value treatment. Default is ignore
#' @param use.maxlr Use MaxLR statistic for split point selection (as proposed by Arnold et al., 2021). This corrects the bias in the LR statistics incurred by testing multiple split points within one variable.
#' @param strucchange.from Strucchange argument. See their package
#' documentation.
#' @param strucchange.to Strucchange argument. See their package documentation.
#' @param strucchange.nrep Strucchange argument. See their package
#' documentation.
#' @param refit If TRUE (default) the initial model is fitted on the data
#' provided to \code{\link{semtree}}.
#' @param ctsem_sd If FALSE (default) no standard errors of CT model parameters
#' are computed. Requesting standard errors increases runtime. 
#' @param loglik  Character. Algorithm to compute log likelihood. The 'default' algorithm 
#' depends on the chosen SEM package. It is 'mvn' for lavaan and 'model' for all
#' other packages. 'model'refers to a model-based computation. 
#' This is preferable because it is more general. As an alternative, 'mvn' computes 
#' the log likelihood based on the
#' multivariate normal density and the model-implied mean and covariance matrix.
#' @return A control object containing a list of the above parameters.
#' @author Andreas M. Brandmaier, John J. Prindle, Manuel Arnold
#' @seealso \code{\link{semtree}}
#' @references Brandmaier, A.M., Oertzen, T. v., McArdle, J.J., & Lindenberger,
#' U. (2013). Structural equation model trees. \emph{Psychological Methods},
#' 18(1), 71-86.
#' @references 
#' Arnold, M., Voelkle, M. C., & Brandmaier, A. M. (2021). Score-guided structural equation model trees. \emph{Frontiers in Psychology}, 11, Article 564403. https://doi.org/10.3389/fpsyg.2020.564403

#' @examples
#' 
#' 
#' 	# create a control object with an alpha level of 1%
#' 	my_control <- semtree_control(alpha=0.01)
#' 
#' 	# set the minimum number of cases per node to ten
#' 	my_control$min.N <- 10
#' 	
#' 	# print contents of the control object
#' 	print(my_control)
#' 
#' 
#' @export
semtree_control <-
  function(method = c("naive","score","fair","fair3"),
           min.N = NULL,
           max.depth = NA,
           alpha = .05,
           alpha.invariance = NA,
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
           #score.tests = list(nominal = 'LMuo',  # currently only LMuo, maxLMo
           #                   ordinal = 'maxLMo', # and maxLM are available
           #                   metric = 'maxLM'),
           linear = TRUE,
           min.bucket = NULL,
           missing = 'ignore',
           use.maxlr = FALSE,
           strucchange.from = 0.15,
           strucchange.to = NULL,
           strucchange.nrep = 50000,
           refit = TRUE,
           ctsem_sd = FALSE,
           loglik = c("default", "model", "mvn"))
  {
    options <- list()
    # verbose output during generation of SEMTree
    options$verbose <- verbose
    # For OpenMx models: Is the model linear?
    ### Global Invariant parameters are currently not working with the speed up
    options$linear <- linear
    # exclude heywood cases from further evaluation
    options$exclude.heywood <- exclude.heywood
    # minimum number of cases for SEM evaluation
    options$min.N <- min.N
    # minimum number of cases in leaf
    options$min.bucket <- min.bucket
    # method
    options$method <- match.arg(method)
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
    # missing data treatment
    options$missing <- missing
    # max LM stat
    options$use.maxlr <- use.maxlr
    # from (for strucchange)
    options$strucchange.from <- strucchange.from
    # to (for strucchange)
    if (is.null(strucchange.to)) {
      strucchange.to <- 1 - strucchange.from
    }
    options$strucchange.to <- strucchange.to
    # nrep (for strucchange)
    options$strucchange.nrep <- strucchange.nrep
    # refit the initial model
    options$refit <- refit
    # should standard errors of CT models be computed? Increases runtime.
    options$ctsem_sd <- ctsem_sd
    # algorithm to compute log likelihood
    options$loglik <- match.arg(loglik)
    
    class(options) <- "semtree.control"
    
    return(options)
  }

#' @deprecated since version 0.10.0
#' @export
semtree.control <- function(...) {
  warning("Calling semtree.control() is deprecated! Please use semtree_control() instead.")
  semtree_control(...)
}