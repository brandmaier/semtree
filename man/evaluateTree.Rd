\name{evaluateTree}
\alias{evaluateTree}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Evaluate Tree -2LL
}
\description{
A helper function to evaluate the -2LL of leaf (terminal) nodes for a dataset. When given a \code{\link{semtree}} and a unique dataset, the model estimates -2LL for the tree parameters and data subsets that fit the tree branching criteria.
}
\usage{
evaluateTree(tree, test_set, data_type = "raw", leaf_ids = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tree}{
A fitted \code{\link{semtree}} object
}
  \item{test_set}{
Dataset to fit to a fitted \code{\link{semtree}} object
}
  \item{data_type}{
type of data ("raw", "cov", "cor")
}
  \item{leaf_ids}{
Identifies which nodes are leaf nodes. Default is NULL, which checks model for leaf nodes and fills this information in automatically.
}
}

\value{
A list with two elements:
\item{deviance}{Combined -2LL for leaf node models of the tree.}
\item{num_models}{Number of leaf nodes used for the deviance calculations.}
%% ...
}
\references{
  Brandmaier, A.M., Oertzen, T. v., McArdle, J.J., & Lindenberger, U. (2013). Structural equation model trees. \emph{Psychological Methods}, 18(1), 71-86.
}
\author{
  Andreas M. Brandmaier, John J. Prindle
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{evaluateDataLikelihood}}, \code{\link{semtree}}, \code{\link{semforest}}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
