\name{semforest}
\alias{semforest}
\alias{print.semforest}
\alias{plot.semforest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create a SEM Forest
}
\description{
Grows a SEM Forest from a template model and a dataset. This may take some time.
}
\usage{
semforest(model, data, control = NULL, predictors = NULL,
                 constraints = NULL, cluster=NULL, ...)
}
% print.semforest(forest)
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
A template SEM. Same as in \code{semtree}.
}
  \item{data}{
A dataframe to create a forest from. Same as in \code{semtree}.
}
  \item{control}{
A semforest control object to set forest parameters.
}
  \item{predictors}{
An optional list of covariates. See semtree code example.
}
  \item{constraints}{
An optional list of covariates. See semtree code example.
}
\item{cluster}{
 An object of class "cluster" representing a parallel socket cluster. See package \link[parallel]{makeCluster}.
}

  \item{\dots}{
Optional parameters.
}
}
\value{
A semforest object.
}
\references{
Brandmaier, A.M., Prindle, J. J., McArdle, J. J., & Lindenberger, U. (2016). Theory-guided exploration with structural equation model forests. \emph{Psychological Methods}, 21(4), 566--582.

Brandmaier, A.M., Oertzen, T. v., McArdle, J.J., & Lindenberger, U. (2013). Structural equation model trees. \emph{Psychological Methods}, 18(1), 71--86.
}
\author{
  Andreas M. Brandmaier, John J. Prindle
}

\seealso{
\code{\link{semtree}}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ tree }
\keyword{ models }
\keyword{ multivariate }
