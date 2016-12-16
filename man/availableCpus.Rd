\name{availableCpus}
\alias{availableCpus}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Available Workspace CPUs
}
\description{
Provides user with the number of CPUs available for analysis in the current work environment.
}
\usage{
availableCpus()
}
%- maybe also 'usage' for other objects documented here.

\value{
Returns a numeric value corresponding to the number of CPUs available.
}
\references{
  Brandmaier, A.M., Oertzen, T. v., McArdle, J.J., & Lindenberger, U. (2013). Structural equation model trees. \emph{Psychological Methods}, 18(1), 71-86.
}
\author{
  Andreas M. Brandmaier, John J. Prindle
}
\note{
Helper function for parallel computing in SEM Forest algorithm.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{semtree}}
}
\examples{
# The function is input with no arguments
availableCpus()
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
