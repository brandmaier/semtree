\name{merge}
\alias{merge.semforest}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Merge two SEM forests
}
\description{
This overrides generic base::merge() to merge two forests into one.
}


\usage{
\method{merge}{semforest}(x, y, ...)
}


\arguments{
  \item{x}{A SEM Forest}
  \item{y}{A second SEM Forest}  
  \item{\ldots}{Extra arguments. Currently unused.}
  }
  \references{
    Brandmaier, A.M., Oertzen, T. v., McArdle, J.J., & Lindenberger, U. (2013). Structural equation model trees. \emph{Psychological Methods}, 18(1), 71-86.
  }
  \author{
    Andreas M. Brandmaier, John J. Prindle
  }
  \seealso{
    \code{\link{semtree}}
  }