\name{semtree.constraints}
\alias{semtree.constraints}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  SEM Tree Constraints Object
}
\description{
A SEM Tree constraints object holds information regarding specifics on how the tree is grown (similar to the control object). The SEM tree control object holds all information that is independent of a specific model whereas the constraints object holds information that is specific to a certain model (e.g., specifies differential treatment of certain parameters, e.g., by holding them constant across the forest).
}


\usage{
semtree.constraints(local.invariance=NULL, global.invariance=NULL, focus.parameters=NULL)
}


\arguments{
  \item{local.invariance}{Vector of parameter names that are locally equal, that is, they are assumed to be equal when assessing a local
  split but allowed to differ subsequently.}
  \item{global.invariance}{Vector of parameter names that are globally equal, that is, estimated only once and then fixed in the tree.}
  \item{focus.parameters}{Vector of parameter names that exclusively are evaluated for between-group differences when assessing split candidates. If NULL all parameters add to the difference.} 
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