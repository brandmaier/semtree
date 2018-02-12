\name{evaluate}
\alias{evaluate.semforest}
\alias{evaluate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Average Deviance of a Dataset given a Forest 
}
\description{
Evaluates the average deviance (-2LL) of a dataset given a forest.
}
\usage{
evaluate(x, data, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A fitted \code{\link{semforest}} object
}
  \item{data}{
A data.frame
	}
\item{\ldots}{ No extra parameters yet.}
}

\value{
Average deviance
}
\references{
  Brandmaier, A.M., Oertzen, T. v., McArdle, J.J., & Lindenberger, U. (2013). Structural equation model trees. \emph{Psychological Methods}, 18(1), 71-86.
}
\author{
  Andreas M. Brandmaier
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{evaluateDataLikelihood}}, \code{\link{semtree}}, \code{\link{semforest}}
}

