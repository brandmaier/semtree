#' @exportS3Method print semtree.control
print.semtree.control <- function(x, ...)
{
	stopifnot(is(x, "semtree.control"))

	cat(" SEM-Tree control:\n",horiz.line(),"\n")

	ui_bullet("Splitting Method: ", x$method,"\n")
	
	if(x$method == "score"){
	  ui_bullet("Score Tests:  ", "nominal = ", x$score.tests[[1]], ", ordinal = ",
	      x$score.tests[[2]], ", metric = ", x$score.tests[[3]],"\n", sep = "")
	  ui_bullet("Information Matrix: ", x$information.matrix, "\n")
	  ui_bullet("Scaled Score: ", x$scaled_scores, "\n")
	  ui_bullet("Linear OpenMx model: ", x$linear)
	}
	
	ui_bullet("Alpha Level: ", x$alpha,"\n");
	ui_bullet("Bonferroni Correction:", x$bonferroni,"\n")
	ui_bullet("Minimum Number of Cases: ", x$min.N,"\n")
	ui_bullet("Maximum Tree Depth: ", x$max.depth,"\n")
	ui_bullet("Number of CV Folds: ", x$num.folds,"\n");
  #ui_bullet("Individual CV Folds: ", x$individual.cv.folds,"\n")
	ui_bullet("Exclude Heywood Cases: ", x$exclude.heywood,"\n")
	ui_bullet("Test Invariance Alpha Level: ", x$alpha.invariance,"\n");
	ui_bullet("Use all Cases: ", x$use.all,"\n");
	ui_bullet("Verbosity: ", x$verbose, "\n")  ;
	ui_bullet("Progress Bar: ", x$progress.bar, "\n")  ;
	if(!is.null(x$seed)){ui_bullet("Seed: ", x$seed, "\n")}
  else{ui_bullet("Seed: uninitialized")}
}
