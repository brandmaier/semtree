semtree.control <-
function(method="naive", min.N = 20, max.depth=NA, alpha=.05, alpha.invariance=NA,
         folds=5, exclude.heywood=TRUE, progress.bar=TRUE, 
         verbose=FALSE, bonferroni=FALSE, use.all=FALSE, seed = NA, custom.stopping.rule=NA,
		 mtry=NA, report.level=0, exclude.code=NA, test.type="ml",
		 score.tests = list(nominal = 'LMuo', ordinal = 'maxLMo', metric = 'CvM'),
		 information.matrix = "info", scaled_scores = TRUE, linear = TRUE,
		 min.bucket=10, naive.bonferroni.type=0)
{
	options <- list()
	# verbose output during generation of SEMTree
	options$verbose <- verbose
	# test type ('ml' or 'score')
	options$test.type <- test.type
	# score tests for each scale type
	options$score.tests <- lapply(X = score.tests, FUN = tolower)
	# information matrix used to decorrelate scores
	options$information.matrix <- information.matrix
	# Scale scores for testing continuous covariates
	options$scaled_scores <- scaled_scores
	# For OpenMx models: Is the model linear?
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
  options$exclude.code <- exclude.code;
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
  
	class(options) <- "semtree.control"
	
	return(options)	
}