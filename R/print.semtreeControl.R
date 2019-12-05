print.semtreeControl <- function(x, ...)
{
	stopifnot(is(x, "semtree.control"))

	cat("SEM-Tree control\n----------------\n")

	cat("Splitting Method: ", x$method,"\n")
	cat("Test Type: ",x$test.type,"\n")
	cat("Alpha Level: ", x$alpha,"\n");
	cat("Bonferroni Correction:", x$bonferroni,"\n")
	cat("Minimum Number of Cases: ", x$min.N,"\n")
	cat("Maximum Tree Depth: ", x$max.depth,"\n")
	cat("Number of CV Folds: ", x$num.folds,"\n");
  #cat("Individual CV Folds: ", x$individual.cv.folds,"\n")
	cat("Exclude Heywood Cases: ", x$exclude.heywood,"\n")
	cat("Test Invariance Alpha Level: ", x$alpha.invariance,"\n");
	cat("Use all Cases: ", x$use.all,"\n");
	cat("Verbosity: ", x$verbose, "\n")  ;
	cat("Progress Bar: ", x$progress.bar, "\n")  ;
	if(!is.null(x$seed)){cat("Seed: ", x$seed, "\n")}
  else{cat("Seed: uninitialized")}
}
