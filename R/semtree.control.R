semtree.control <-
function(method="naive", min.N = 20, max.depth=NA, alpha=.05, alpha.invariance=NA,
         folds=5, exclude.heywood=TRUE, progress.bar=TRUE, 
         verbose=FALSE, bonferroni=FALSE, use.all=FALSE, seed = NA, custom.stopping.rule=NA,
		 mtry=NA, report.level=0, exclude.code=NA )
{
	options <- list()
	# verbose output during generation of SEMTree
	options$verbose <- verbose
	# number of cross validation folds
	options$num.folds <- folds
	# individual CV folds for data under missingess (should be no option in the long run)
	#options$individual.cv.folds <- TRUE
	# exclude heywood cases from further evaluation
	options$exclude.heywood <- exclude.heywood
	# minimum number of cases for SEM evaluation
	options$min.N <- min.N
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
  
	class(options) <- "semtree.control"
	
	return(options)	
}
