\name{sfMapply}
\alias{sfMapply}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Parallel version of mapply using Snowfall
}
\description{
sfMapply is a multivariate version of sfApply. sfMapply applies FUN to the first elements of each ... argument, the second elements, the third elements, and so on. Arguments are recycled if necessary.
}
\usage{
sfMapply(FUN, ..., MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{FUN}{
Function to call.
}
  \item{\dots}{
arguments to vectorize over.
}
  \item{MoreArgs}{
Arguments passed to FUN.
}
  \item{SIMPLIFY}{
logical or character string; attempt to reduce the result to a vector, matrix or higher dimensional array
}
  \item{USE.NAMES}{
logical; use names if the first ... argument has names, or if it is a character vector, use that character vector as the names.
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
