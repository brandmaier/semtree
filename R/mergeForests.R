#' Merge two SEM forests
#' 
#' This overrides generic base::merge() to merge two forests into one.
#' 
#' 
#' @aliases merge.semforest
#' @param x A SEM Forest
#' @param y A second SEM Forest
#' @param \dots Extra arguments. Currently unused.
#' @author Andreas M. Brandmaier, John J. Prindle
#' @seealso \code{\link{semtree}}
#' @references Brandmaier, A.M., Oertzen, T. v., McArdle, J.J., & Lindenberger,
#' U. (2013). Structural equation model trees. \emph{Psychological Methods},
#' 18(1), 71-86.
#' @exportS3Method merge semforest
merge.semforest <- function(x, y, ...)
{
  return(merge.internal(list(x,y)))
}
  
  
merge.internal <- function(forest.list){
  
  # determine number of forests to merge
  num.forests <- length(forest.list)
  
  # iterate through all forests and merge them with the first
  forest <- forest.list[[1]]
  for (i in 2:num.forests) {
    # check whether models are compatible
    m1 <- forest$model
    m2 <- forest.list[[i]]$model
    if (getModelType(m1) != getModelType(m2)) stop("Incompatible models")
    if (getModelType(m1)=="OpenMx") {
      # for OpenMx models, we compare whether a selected set of
      # attributes instead of the entire object because eg. 
      # the output-attribute may differ on time stamps or
      # the compute-attribute may differ for the optimizer used or
      # the number of iterations
      c1 <- TRUE
      for (at in list("matrices","algebras","constraints","latentVars","manifestVars",
                      "data","data means","data type","submodels","expectation","fitfunction",
                      "independent")) {
        c1_temp <- digest::digest(attr(m1,at))==digest::digest(attr(m2,at))
        if (!c1_temp) { ui_warn("Models differ on attribute '",at,"'.") }
        c1 <- c1 & c1_temp
      }
    } else if (getModelType(m1)=="lavaan") {
      c1 <- digest::digest(m1)==digest::digest(m2)
    } else {
      c1 <- digest::digest(m1)==digest::digest(m2)
    }
    # some checks
    
    tmp1 <- forest$control
    tmp1$num.trees <- NA
    tmp2 <- forest.list[[i]]$control
    tmp2$num.trees <- NA
    c2 <- digest::digest(tmp1)==digest::digest(tmp2)
    if (!c1) {
      stop("Cannot merge forests! Models differ.");
    }
    
    if (!c2) {
      warning("Merging forests with different control objects!");
    }
    
    forest$forest <- c(forest$forest,forest.list[[i]]$forest)
    forest$forest.data <- c(forest$forest.data,forest.list[[i]]$forest.data)
    forest$seeds <- c(forest$forest.seeds,forest.list[[i]]$forest.seeds)
    forest$elapsed <- forest$elapsed+forest.list[[i]]$elapsed
  }
  
  forest$merged <- TRUE
  
  return(forest)
}

