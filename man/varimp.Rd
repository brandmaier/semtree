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
varimp(forest, var.names = NULL, verbose = F, main.effects = F, 
	cluster = NULL, eval.fun = evaluateTree, ...)
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
  \item{main.effects}{
Boolean to only examine the first split in all trees.
}
  \item{eval.fun}{
Default is \code{\link{evaluateTree}} function. The value of the -2LL of the leaf nodes is compared to baseline overall model.
}
\item{cluster}{
 An object of class "cluster" representing a parallel socket cluster. See package \link[parallel]{makeCluster}.
}
\item{\ldots}{Optional arguments.}
}

\references{
  Brandmaier, A.M., Oertzen, T. v., McArdle, J.J., & Lindenberger, U. (2013). Structural equation model trees. \emph{Psychological Methods}, 18(1), 71-86.
}
\author{
  Andreas M. Brandmaier, John J. Prindle
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
