\name{proximity}
\alias{proximity}
\alias{plot.proximity}
\alias{plot.semforest.proximity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
SEM Forest Case Proximity
}
\description{
A \code{\link{semforest}} process to represent proportion of trees where each case are in the same leaf nodes. The values are bounded (0,1), where higher values are closer in proximity. 
}
\usage{
proximity(forest, dataset = NULL, type = 0, aggregate = T, cluster = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
% plot.proximity(prox, main = "Principal Coordinates", xlim = NULL, ylim = NULL, col = "black", pch = NULL, axes = c(1, 2), type = "mds", cex = 2, ...)

\arguments{
  \item{forest}{
A \code{\link{semforest}} object.
}
  \item{dataset}{
A dataset to compute proximity values for.
}
  \item{type}{
Missingness accounted for. (0 = no, 1 = yes)
}
  \item{aggregate}{
Boolean marker to compute aggregate proximity scores.
}
\item{cluster}{
 An object of class "cluster" representing a parallel socket cluster. See package \link[parallel]{makeCluster}.
}
\item{\ldots}{Optional arguments.}
}

\value{
A matrix with dimensions NxN is returned. The values of each cell are bounded (0,1) and represent proportion of trees where each case are in the same leaf nodes.
}
\references{
  Brandmaier, A.M., Oertzen, T. v., McArdle, J.J., & Lindenberger, U. (2013). Structural equation model trees. \emph{Psychological Methods}, 18(1), 71-86.
}
\author{
  Andreas M. Brandmaier, John J. Prindle
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{semforest}}, \code{\link{semtree}}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
