\name{prune}
\alias{prune.semtree}
\alias{prune.semforest}
\alias{prune}
\title{Prune a SEM Tree or SEM Forest}

\description{
  Returns a new tree with a maximum depth selected by the user. can be used in conjunction with plot commands to view various pruning levels.
}
\usage{
  prune(object, ...)
}
\arguments{
  \item{object}{A \code{\link{semtree}} or semforest object.}
%  \item{max.depth}{The number of levels desired for a SEM Tree to grow. This must be less than the max depth defined in the \code{\link{semtree.control}} and/or the max depth obtained in the \code{\link{semtree}} analysis.}
%  \item{num.trees}{Limit to a number of trees.}
  \item{\dots}{Optional parameters, such as \code{max.depth} the maximum depth of each tree, or also \code{num.trees} when pruning a forest.}
}
\details{
  The returned tree is only modified by the number of levels for the tree. This function does not reevaluate the data, but provides alternatives to reduce tree complexity. If the user would like to alter the tree by increasing depth, then max.depth option must be adjusted in the \code{\link{semtree.control}} object (provided further splits are able to be computed).
}
\value{
  Returns a \code{\link{semtree}} object.
}
\references{
  Brandmaier, A.M., Oertzen, T. v., McArdle, J.J., & Lindenberger, U. (2013). Structural equation model trees. \emph{Psychological Methods}, 18(1), 71-86.
}
\author{
  Andreas M. Brandmaier, John J. Prindle
}
\seealso{
  \code{\link{semtree}},
  \code{\link{semtree.control}}
}
