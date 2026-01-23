#' SEM Forest Variable Importance
#' 
#' A function to calculate relative variable importance for selecting node
#' splits over a \code{\link{semforest}} object.
#' 
#' 
#' @aliases varimp plot.semforest.varimp varimpConvergencePlot
#' print.semforest.varimp
#' @param forest A \code{\link{semforest}} object
#' @param var.names Covariates used in the forest creation process. NULL value
#' will be automatically filled in by the function.
#' @param verbose Boolean to print messages while function is running.
#' @param method Character. Define the method, with which importance is computed. The default is NULL and picks the appropriate permutation-based estimation method depending on whether no focus parameters are given ("permutation") or focus parameters are given ("permutationFocus")
#' @param eval.fun Default is \code{\link{evaluateTree}} function. The value of
#' the -2LL of the leaf nodes is compared to baseline overall model.
#' @param conditional Conditional variable importance if TRUE, otherwise
#' marginal variable importance.
#' @param strict Boolean. Default is TRUE. Only consider estimates from models if there were no model convergence problems. Otherwise, partial results are used, which may incur some downward bias.
#' @param \dots Optional arguments.
#' @author Andreas M. Brandmaier, John J. Prindle
#' @references Brandmaier, A.M., Oertzen, T. v., McArdle, J.J., & Lindenberger,
#' U. (2013). Structural equation model trees. \emph{Psychological Methods},
#' 18(1), 71-86.
#' 
#' @export
#' 
varimp <- function(forest,
                   var.names = NULL,
                   verbose = FALSE,
                   #                   main.effects = F,
                   eval.fun = evaluateTree,
                   method = NULL,
                   conditional = FALSE,
                   strict = TRUE,
                   ...)
{
  if (is.null(var.names)) {
    var.names <- forest$covariates
  }
  
  has_focus_params =  !is.null(forest$constraints$focus.parameters)
  
  # choose appropriate method if no explicit method is given
  if (is.null(method)) {
    if (has_focus_params) {
      method="permutationFocus"
    } else {
      method="permutation"      
    }
  }
  
  if (method == "permutation" && has_focus_params)
  {
    ui_warn(
      "Consider switching to method='permutationFocus' because forest has focus parameters."
    )
    
    #ui_warn("Switching to method='permutationFocus' because forest has focus parameters.")
    #method = "permutationFocus"
  }
  
  
  
  result <- list()
  start.time <- proc.time()
  nullforests <- sapply(forest$forest, is.null)
  if(all(nullforests)) stop("No valid trees in the forest.")
  if(any(nullforests)) ui_warn("Removing ", sum(nullforests), " invalid trees from the forest of ", length(nullforests), " trees.")
  forest$forest <- forest$forest[!nullforests]
  temp <- future.apply::future_mapply(
    FUN = varimpTree,
    forest$forest,
    forest$forest.data,
    MoreArgs = list(
      var.names = var.names,
      verbose = verbose,
      max.level = NA,
      eval.fun = eval.fun,
      method = method,
      constraints = forest$constraints,
      loglik = forest$control$semtree.control$loglik,
      strict = strict
    ),
    SIMPLIFY = FALSE,
    USE.NAMES = TRUE,
    future.seed = TRUE
  )

  elapsed <- proc.time() - start.time

  num.trees <- length(temp)
  num.vars <- length(var.names)
  result$ll.baselines <- numeric(num.trees)
  result$importance <- matrix(NA_real_, nrow = num.trees, ncol = num.vars)

  for (i in seq_len(num.trees)) {
    tree.res <- temp[[i]]
    result$ll.baselines[[i]] <- tree.res$ll.baseline
    result$importance[i, ] <- tree.res$total
  }

  result$elapsed <- elapsed
  
  # completeley experimental, probably not a wise idea to use this
  if (method == "prune") {
    temp <- future.apply::future_mapply(
      varimpTree,
      forest$forest,
      forest$forest.data,
      MoreArgs = list(
        var.names = var.names,
        verbose = verbose,
        max.level = 1
      ),
      SIMPLIFY = FALSE,
      USE.NAMES = T
    )
    
    result$importance.level1 <-
      t(sapply(temp, function(x) {
        x$total
      }))
    colnames(result$importance.level1) <- var.names
  }
  
  # at this stage, result$importance should be a k x p matrix
  # with k being the number of trees and p being the number
  # of predictors
  # result$ll.baselines is a vector of length k
 
  # in case, the result is only from a single predictor, 
  # then transpose results accordingly such that is k x p 
  if (length(var.names) == 1) {
    result$importance<-matrix(result$importance, ncol=1)
  
   # result$ll.baselines <-
  #    matrix(result$ll.baselines, ncol=1) 
   }
  
  colnames(result$importance) <- var.names
  result$var.names <- var.names
  class(result) <- "semforest.varimp"
  return(result)
  
  
}


