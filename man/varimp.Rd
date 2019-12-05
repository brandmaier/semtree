\name{varimp}
\alias{varimp}
\alias{plot.semforest.varimp}
\alias{varimpConvergencePlot}
\alias{print.semforest.varimp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
SEM Forest Variable Importance
}
\description{
A function to calculate relative variable importance for selecting node splits over 
a \code{\link{semforest}} object. 
}
\usage{
varimp(forest, var.names = NULL, verbose = F,  
	cluster = NULL, eval.fun = evaluateTree, method = "permutation", 
	conditional=FALSE, ...)
}


%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{forest}{
A \code{\link{semforest}} object
}
  \item{var.names}{
Covariates used in the forest creation process. NULL value will be automatically filled in by the function.
}
  \item{verbose}{
Boolean to print messages while function is running.
}
  \item{method}{
Experimental. Some alternative methods to compute importance. Default is "permutation".
}
  \item{eval.fun}{
Default is \code{\link{evaluateTree}} function. The value of the -2LL of the leaf nodes is compared to baseline overall model.
}
\item{cluster}{
 An object of class "cluster" representing a parallel socket cluster. See package \link[parallel]{makeCluster}.
}
\item{conditional}{
  Conditional variable importance if TRUE, otherwise marginal variable importance.
}
\item{\ldots}{Optional arguments.}
}

\references{
  Brandmaier, A.M., Oertzen, T. v., McArdle, J.J., & Lindenberger, U. (2013). Structural equation model trees. \emph{Psychological Methods}, 18(1), 71-86.
}
\author{
  Andreas M. Brandmaier, John J. Prindle
}
