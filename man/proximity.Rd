% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/proximity.R
\name{proximity}
\alias{proximity}
\title{Compute proximity matrix}
\usage{
proximity(x, data, ...)
}
\arguments{
\item{x}{An object for which a method exists.}

\item{data}{A data.frame on which proximity is computed}

\item{...}{Parameters passed to other functions.}
}
\value{
A matrix with dimensions [i, j] whose elements reflect the proportion
of times case i and j were in the same terminal node of a tree.
}
\description{
Compute a n by n matrix across all trees in a forest,
where n is the number of rows in the
data, reflecting the proportion of times two cases ended up in the same
terminal node of a tree.
}
\details{
SEM Forest Case Proximity
}
\examples{
nodeids <- structure(c(9, 3, 5, 7, 10, 4, 6, 8, 9, 3, 5, 7, 10, 4, 6, 8),
.Dim = c(4L, 4L))
class(nodeids) <- "semforest_node_id"
sims <- proximity(nodeids)
dd <- as.dist(1-sims)
hc <- hclust(dd)
groups <- cutree(hc, 2)
}
\references{
Brandmaier, A.M., Oertzen, T. v., McArdle, J.J., & Lindenberger,
U. (2013). Structural equation model trees. \emph{Psychological Methods},
18(1), 71-86.
}
\seealso{
\code{\link{semforest}}, \code{\link{semtree}}
}
\author{
Caspar J. Van Lissa, Andreas M. Brandmaier, John J. Prindle
}
