\name{biodiversity}
\alias{biodiversity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Quantify bio diversity of a SEM Forest
}
\description{
A function to calculate biodiversity of a \code{\link{semforest}} object. 
}
\usage{
biodiversity(x, aggregate.fun=median)
}


%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A \code{\link{semforest}} object
}
  \item{aggregate.fun}{
Takes a function to apply to the vector of pairwise diversities. By default, this is the median.
}
}

\author{
  Andreas M. Brandmaier
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ biodiversity }
\keyword{ semforest }% __ONLY ONE__ keyword per line
