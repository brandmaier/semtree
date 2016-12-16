\name{subforest}
\alias{subforest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Creates subsets of trees from forests
}
\description{
Creates subsets of a forest. This can be used to subset a number of trees, e.g. from:(from+num), or to remove all null (type="nonnull") trees that were due to errors, or to randomly select a sub forest (type=random).
}
\usage{
subforest(forest, num = NULL, type = "nonnull", from = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{forest}{
A SEM Forest object.
}
  \item{num}{
Number of trees to select.
}
  \item{type}{
Either 'random' or 'nonnull' or NULL. First selects a random subset, second selects all non-null trees, third allows subsetting trees. 
}
  \item{from}{
Starting index if type=NULL.
}
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
