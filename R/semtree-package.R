#' @title SEM Tree Package
#' @name semtree-package
#' @importFrom stats as.formula predict
#' @importFrom lavaan lavScores nobs vcov
#' @importFrom utils toLatex
#' @importFrom zoo zoo
#' @importFrom strucchange catL2BB maxBB meanL2BB ordL2BB ordwmax root.matrix sctest supLM
#' @importFrom sandwich bread
#' @importFrom methods is
#' @importFrom parallel parLapply clusterMap
#' @importFrom utils flush.console getS3method sessionInfo str setTxtProgressBar data
#' @importFrom stats as.dist cmdscale coef cor cov logLik median pchisq qnorm runif var dist rnorm
#' @importFrom graphics barplot legend lines pairs par plot strwidth text hist
#' @import OpenMx
#' @importFrom bitops bitAnd
#' @import rpart
#' @importFrom methods hasArg
#' @importFrom data.table data.table
.SCALE_METRIC = 2
.SCALE_ORDINAL = 3
.SCALE_CATEGORICAL = 1



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









