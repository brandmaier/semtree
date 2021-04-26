#' @title SEM Tree Package
#' @importFrom stats as.formula predict
#' @importFrom lavaan lavScores nobs vcov
#' @importFrom utils toLatex
#' @importFrom zoo zoo
#' @importFrom strucchange catL2BB maxBB meanL2BB ordL2BB ordwmax root.matrix sctest supLM
#' @importFrom sandwich bread
#' @importFrom methods is
#' @importFrom sets as.set
#' @importFrom parallel parLapply clusterMap
#' @importFrom utils flush.console getS3method sessionInfo str setTxtProgressBar data
#' @importFrom stats as.dist cmdscale coef cor cov logLik median pchisq qnorm runif var dist rnorm
#' @importFrom graphics barplot legend lines pairs par plot strwidth text hist
#' @import OpenMx
#' @importFrom bitops bitAnd
#' @import rpart
NULL






#' Simulated Linear Latent Growth Curve Data
#' 
#' This data set provides simple data to fit with a LGCM.
#' 
#' 
#' @name lgcm
#' @docType data
#' @format \code{lgcm} is a matrix containing 400 rows and 8 columns of
#' simulated data. Longitudinal observations are o1-o5. Covariates are
#' agegroup, training, and noise.
#' @author Andreas M. Brandmaier \email{brandmaier@@mpib-berlin.mpg.de}
#' @keywords datasets
NULL





#' Merge two SEM forests
#' 
#' This overrides generic base::merge() to merge two forests into one.
#' 
#' 
#' @aliases merge.semforest
#' @param x A SEM Forest
#' @param y A second SEM Forest
#' @param list() Extra arguments. Currently unused.
#' @author Andreas M. Brandmaier, John J. Prindle
#' @seealso \code{\link{semtree}}
#' @references Brandmaier, A.M., Oertzen, T. v., McArdle, J.J., & Lindenberger,
#' U. (2013). Structural equation model trees. \emph{Psychological Methods},
#' 18(1), 71-86.
NULL





#' SEMtrees Parameter Estimates Standard Error Table
#' 
#' Returns a table of standard errors with columns corresponding to freely
#' estimated standard errors and rows corresponding to nodes in the tree.
#' 
#' The row names of the resulting data frame correspond to internal node ids
#' and the column names correspond to standard errors in the SEM. Parameter
#' estimates can be obtained from \code{\link{parameters}}.
#' 
#' @aliases se
#' @param tree A SEMtree object obtained from \code{\link{semtree}}
#' @param leafs.only Default = TRUE. Only the terminal nodes (leafs) are
#' printed. If set to FALSE, all node standard errors are written to the
#' \code{data.frame}.
#' @return Returns a \code{data.frame} with rows for parameters and columns for
#' terminal nodes.
#' @author Andreas M. Brandmaier, John J. Prindle
#' @seealso \code{\link{semtree}}, \code{\link{semtree.control}},
#' \code{\link{parameters}}
#' @references Brandmaier, A.M., Oertzen, T. v., McArdle, J.J., & Lindenberger,
#' U. (2013). Structural equation model trees. \emph{Psychological Methods},
#' 18(1), 71-86.
NULL



