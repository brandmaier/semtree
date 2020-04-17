\name{getLeafs}
\alias{getLeafs}
\title{
  Get a list of all leafs in a tree
}
\description{
  Get a list of all leafs in a tree by recursively searching the tree starting at the given node (if not \code{data} object is given. If \code{data} is given, the function returns the leafs that are predicted for each row of the given data.
}
\usage{
  getLeafs(tree, data=NULL)
}


%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tree}{
    A \code{\link{semtree}} object
  }
  \item{data}{
  	A \code{data.frame} 
  }
}


\references{
  Brandmaier, A.M., Oertzen, T. v., McArdle, J.J., & Lindenberger, U. (2013). Structural equation model trees. \emph{Psychological Methods}, 18(1), 71-86.
}
\author{
  Andreas M. Brandmaier
}
