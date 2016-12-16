\name{findOtherSplits}
\alias{findOtherSplits}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find Other Node Split Values
}
\description{
Search tool to search nodes for alternative splitting values found during the \code{\link{semtree}} process. Given a particular node, competing split values are listed assuming they also meet the criteria for a significant splitting value as set by \code{\link{semtree.control}}. 
}
\usage{
findOtherSplits(node, tree)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{node}{
A node from a \code{\link{semtree}} object.
}
  \item{tree}{
A \code{\link{semtree}} object which the node is part of.
}
}

\value{
A \code{data.frame()} with rows corresponding to the variable names and split values for alternative splits found in the node of interest.
%% ...
}
\references{
  Brandmaier, A.M., Oertzen, T. v., McArdle, J.J., & Lindenberger, U. (2013). Structural equation model trees. \emph{Psychological Methods}, 18(1), 71-86.
}
\author{
  Andreas M. Brandmaier, John J. Prindle
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
